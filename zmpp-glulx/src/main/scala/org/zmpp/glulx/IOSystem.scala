/*
 * Created on 2010/04/07
 * Copyright (c) 2010-2011, Wei-ju Wu.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of Wei-ju Wu nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.zmpp.glulx

import scala.annotation.switch
import org.zmpp.base.Memory
import org.zmpp.glk.Glk
import java.util.logging._

class StreamStrState(val status: Int, var byteAddress: Int, var bitnum: Int) {
  def this(status: Int) = this(status, 0, 0)  
  def startString     = status == StreamStrState.StatusNew
  def printSubstring  = status == StreamStrState.StatusSubstring
  def resumeDecoding  = status == StreamStrState.StatusResume
  def resumeCString   = status == StreamStrState.StatusResumeCString
  def resumeUniString = status == StreamStrState.StatusResumeUniString
  def suspendPrinting = {
    status == StreamStrState.StatusFinished ||
    status == StreamStrState.StatusCallFunction
  }
}

object StreamStrState {
  val StatusNew             = 0
  val StatusContinue        = 1
  val StatusCallFunction    = 2
  val StatusSubstring       = 3
  val StatusFinished        = 4
  val StatusResume          = 5
  val StatusResumeCString   = 6 // this state is generated by FilterIO
  val StatusResumeUniString = 7 // this state is generated by FilterIO 

  // parameterizable singletons
  private val _newString  = new StreamStrState(StatusNew)
  private val _substring  = new StreamStrState(StatusSubstring)
  private val _resume     = new StreamStrState(StatusResume)
  private val _resumecstring  = new StreamStrState(StatusResumeCString)
  private val _resumeunistring  = new StreamStrState(StatusResumeUniString)
  val Continue     = new StreamStrState(StatusContinue)
  val Finished     = new StreamStrState(StatusFinished)
  val CallFunction = new StreamStrState(StatusCallFunction)

  def newString(addr: Int) = {
    _newString.byteAddress = addr
    _newString
  }
  def substring(byteAddr: Int) = {
    _substring.byteAddress = byteAddr
    _substring.bitnum      = 0
    _substring
  }
  def resumeAt(byteAddr: Int, bitnum: Int) = {
    _resume.byteAddress = byteAddr
    _resume.bitnum      = bitnum
    _resume
  }
  def resumeCStringAt(byteAddr: Int) = {
    _resumecstring.byteAddress = byteAddr
    _resumecstring
  }
  def resumeUniStringAt(byteAddr: Int) = {
    _resumeunistring.byteAddress = byteAddr
    _resumeunistring
  }
}

/**
 * The super class of IO systems, it defines the public/overridable methods
 * for streamstr, streamnum, streamchar and streamunichar and also contains
 * the huffman decoding algorithm.
 *
 * Design notes:
 * - we do not build an explicit tree structure and walk the memory structure
 *   directly (simpler and more general, to support different environments)
 * - externalized state: State is not kept in the decoder class, instead,
 *   print state objects are passed within the print process and in the suspend
 *   and resume cycle
 * - use of polymorphism: the Strategy pattern was used to implement different
 *   behaviors of the IOsystems.
 * - Huffman decoding incorporated: the Huffman decompression algorithm was
 *   originally in its own class, but was merged into IOSystem, because it
 *   also behaves different depending on the IOSystem
 *
 * Open question:
 * - what advantages would changing the state objects to use the State pattern
 *   have ?
 */
abstract class IOSystem(vm: GlulxVM, val rock: Int) {
  val logger = Logger.getLogger("glulx")
  def id: Int
  def streamChar(c: Char)
  def streamUniChar(c: Int)
  
  /*
   * Hm, this seems kind of not-documented: non-filter stream_num can also start
   * at any position.
   */
  def streamNum(num: Int, pos: Int) {
    val numberString = "%d".format(num)
    var i = pos
    while (i < numberString.length) {
      streamChar(numberString.charAt(i))
      i += 1
    }
    if (pos > 0) {
      val fpVal    = vm.state.stack.popInt
      val pcVal    = vm.state.stack.popInt
      val destAddr = vm.state.stack.popInt
      val destType = vm.state.stack.popInt
      if (destType == DestTypes.ResumeExecuteFunction) {
        vm.state.pc = pcVal
        vm.state.fp = fpVal
      } else {
        throw new IllegalStateException("FALLBACK, SHOULD NOT HAPPEN, destType is: %d".format(destType))
      }
    }
  }

  def streamStr(state: StreamStrState) {
    var currentState = state
    while (!currentState.suspendPrinting) {
      currentState = streamStrIteration(currentState)
    }
  }

  private def popCallStubIfNecessary(inBetween: Boolean, state: StreamStrState) = {
    if (inBetween && state == StreamStrState.Finished) {
      val fpVal    = vm.state.stack.popInt
      val pcVal    = vm.state.stack.popInt
      val destAddr = vm.state.stack.popInt
      val destType = vm.state.stack.popInt
      if (destType == DestTypes.ResumeExecuteFunction) {
        vm.state.pc = pcVal
        vm.state.fp = fpVal
        StreamStrState.Finished
      } else if (destType == DestTypes.ResumePrintCompressed) {
        StreamStrState.resumeAt(pcVal, destAddr)
      } else {
        logger.severe("FALLBACK, SHOULD NOT HAPPEN, destType is: %d".format(destType))
        StreamStrState.Finished
      }
    } else state
  }

  private def streamStrIteration(state: StreamStrState): StreamStrState = {
    val inBetween = state.printSubstring || state.resumeDecoding ||
                    state.resumeCString || state.resumeUniString
    var nextState = if (state.startString || state.printSubstring) {
      // this conditional branch processes strings only from their start position
      val stringAddress = state.byteAddress
      val strtype = vm.memByteAt(stringAddress)
      strtype match {
        case 0xe0 => handleStreamstrCString(inBetween, stringAddress)
        case 0xe1 => decompress(inBetween, stringAddress + 1, 0)
        case 0xe2 => handleStreamstrUnicodeString(inBetween, stringAddress)
        case _    =>
          throw new IllegalStateException("unknown string type: %02x".format(strtype))
      }
    } else if (state.resumeDecoding) { // continue Huffman decoding in the middle
      decompress(inBetween, state.byteAddress, state.bitnum)
    } else if (state.resumeCString) {
      handleResumeCString(state.byteAddress)
    } else if (state.resumeUniString) {
      handleResumeUniString(state.byteAddress)
    } else throw new IllegalArgumentException("Illegal continuation status: " + state.status)      
    popCallStubIfNecessary(inBetween, nextState)
  }

  def handleChar8(c: Char, inBetween: Boolean, currentStreamByte: Int,
                  currentStreamBit: Int): StreamStrState
  def handleChar32(c: Int, inBetween: Boolean, currentStreamByte: Int,
                   currentStreamBit: Int): StreamStrState

  def handleStreamstrCString(inBetween: Boolean, stringAddress: Int) = {
    var currentAddr = stringAddress + 1
    var currentChar = vm.memByteAt(currentAddr)
    while (currentChar != 0) {
      streamChar(currentChar.asInstanceOf[Char])
      currentAddr += 1
      currentChar = vm.memByteAt(currentAddr)
    }
    StreamStrState.Finished
  }
  
  def handleStreamstrUnicodeString(inBetween: Boolean, stringAddress: Int) = {
    var currentAddr = stringAddress + 4
    var currentChar = vm.memIntAt(currentAddr)
    while (currentChar != 0) {
      streamUniChar((currentChar & 0xffff).asInstanceOf[Char])
      currentAddr += 4
      currentChar = vm.memIntAt(currentAddr)
    }
    StreamStrState.Finished
  }
  
  def handleResumeUniString(addr: Int):StreamStrState = {
    throw new UnsupportedOperationException("resuming Unicode string not supported by default")
  }
  def handleResumeCString(addr: Int): StreamStrState = {
    throw new UnsupportedOperationException("resuming Unicode string not supported by default")
  }

  // ************************************************************************
  // ****** Huffman Decoding
  // ******************************

  private def isStringType(valType: Int) = valType >= 0xe0
  
  private def suspendPrintCompressed(ref: Int,
                                     inBetween: Boolean,
                                     currentStreamByte: Int, currentStreamBit: Int) = {
    if (!inBetween) {
      vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                            vm.state.pc, vm.state.fp)
    }
    vm.state.pushCallStub(DestTypes.ResumePrintCompressed, currentStreamBit,
                          currentStreamByte, vm.state.fp)
    vm.memByteAt(ref)
  }

  private def handleIndirectReference(ref: Int,
                                      inBetween: Boolean,
                                      currentStreamByte: Int,
                                      currentStreamBit: Int): StreamStrState = {
    val refType = suspendPrintCompressed(ref, inBetween, currentStreamByte, currentStreamBit)
    if (isStringType(refType)) {
      StreamStrState.substring(ref)
    } else {
      vm.callWithArgsNoCallStub(ref)
      StreamStrState.CallFunction
    }
  }

  private def handleIndirectReferenceWithArgs(ref: Int, args: Array[Int],
                                              inBetween: Boolean,
                                              currentStreamByte: Int,
                                              currentStreamBit: Int): StreamStrState = {
    val refType = suspendPrintCompressed(ref, inBetween, currentStreamByte, currentStreamBit)
    if (isStringType(refType)) {
      if (refType != 0xe1) throw new UnsupportedOperationException("Uncompressed strings not supported in substrings")
      StreamStrState.substring(ref)
    } else {
      vm.callWithArgsNoCallStub(ref, args)
      StreamStrState.CallFunction
    }
  }
  
  def handleHuffmanCString(nodeAddr: Int,
                           currentStreamByte: Int, currentStreamBit: Int,
                           inBetween: Boolean): StreamStrState
  def handleHuffmanUnicodeString(nodeAddr: Int,
                                 currentStreamByte: Int, currentStreamBit: Int,
                                 inBetween: Boolean): StreamStrState

  private def handleLeaf(nodeType: Int, nodeAddr: Int,
                         inBetween: Boolean,
                         currentStreamByte: Int, currentStreamBit: Int): StreamStrState = {
    (nodeType: @switch) match {
      case 0x02 => // C character
        handleChar8(vm.memByteAt(nodeAddr + 1).asInstanceOf[Char],
                    inBetween, currentStreamByte, currentStreamBit)
      case 0x03 => handleHuffmanCString(nodeAddr,
                                        currentStreamByte, currentStreamBit,
                                        inBetween)
      case 0x04 => // unicode character
        handleChar32(vm.memIntAt(nodeAddr + 1),
                     inBetween, currentStreamByte, currentStreamBit)
      case 0x05 => handleHuffmanUnicodeString(nodeAddr,
                                              currentStreamByte, currentStreamBit,
                                              inBetween)
      case 0x08 => // indirect reference
        handleIndirectReference(vm.memIntAt(nodeAddr + 1),
                                inBetween,
                                currentStreamByte,
                                currentStreamBit)
      case 0x09 => // double indirect reference
        val refptr = vm.memIntAt(nodeAddr + 1)
        handleIndirectReference(vm.memIntAt(refptr),
                                inBetween,
                                currentStreamByte,
                                currentStreamBit)
      case 0x0a => // indirect reference with arguments
        val ref = vm.memIntAt(nodeAddr + 1)
        val argCount = vm.memIntAt(nodeAddr + 5)
        val args = new Array[Int](argCount)
        for (i <- 0 until argCount) {
          args(i) = vm.memIntAt(nodeAddr + 9 + i * 4)
        }
        handleIndirectReferenceWithArgs(ref, args,
                                        inBetween,
                                        currentStreamByte,
                                        currentStreamBit)
      case 0x0b => // double indirect reference with arguments
        val refptr = vm.memIntAt(nodeAddr + 1)
        val argCount = vm.memIntAt(nodeAddr + 5)
        val args = new Array[Int](argCount)
        for (i <- 0 until argCount) {
          args(i) = vm.memIntAt(nodeAddr + 9 + i * 4)
        }
        handleIndirectReferenceWithArgs(vm.memIntAt(refptr), args,
                                        inBetween,
                                        currentStreamByte,
                                        currentStreamBit)            
      case _ =>
        throw new IllegalArgumentException("unsupported node type: %02x".format(nodeType))
    }
  }

  /*
   * This is a stateful computation: After decompress() exits due to a function
   * call, another call to decompress() picks up decoding from where the
   * previous decompress() invocation left off.
   */
  private def decompress(inBetween: Boolean, streamByte: Int, bit: Int): StreamStrState = {
    val decodeTable      = vm.currentDecodingTable
    val tableLength      = vm.memIntAt(decodeTable)
    val numNodes         = vm.memIntAt(decodeTable + 4)
    val rootNodeAddr     = vm.memIntAt(decodeTable + 8)
    var streamAddr       = streamByte
    var bitnum           = bit
    
    var functionWasCalled = false
    var currentNodeAddr   = rootNodeAddr
    var result            = StreamStrState.Continue

    // this value holds the byte sized window into the bitstream
    var currentByte      = vm.memByteAt(streamAddr) >> bitnum

    while (result == StreamStrState.Continue) {
      if ((currentByte & 1) == 0) {
        currentNodeAddr = vm.memIntAt(currentNodeAddr + 1)
      } else {
        currentNodeAddr = vm.memIntAt(currentNodeAddr + 5)
      }
      val nodeType = vm.memByteAt(currentNodeAddr)
      val atLeaf   = nodeType != 0x00
      if (nodeType == 0x01) result = StreamStrState.Finished
      
      // next bit in stream
      if (result == StreamStrState.Continue) {
        bitnum += 1
        currentByte >>= 1
        if (bitnum == 8) {
          bitnum = 0
          streamAddr += 1
          currentByte = vm.memByteAt(streamAddr)
        }
      
        if (atLeaf) {
          result = handleLeaf(nodeType, currentNodeAddr,
                              inBetween, streamAddr, bitnum)
          currentNodeAddr   = rootNodeAddr
        }
      }
    }
    result
  }
}

class NullIOSystem(vm: GlulxVM, rock: Int) extends IOSystem(vm, rock) {
  def id = 0
  def streamChar(c: Char) { }
  def streamUniChar(c: Int) { }
  
  // streamstr actions
  def handleChar8(c: Char, inBetween: Boolean, currentStreamByte: Int,
                  currentStreamBit: Int)    = StreamStrState.Continue
  def handleChar32(c: Int, inBetween: Boolean, currentStreamByte: Int,
                   currentStreamBit: Int) = StreamStrState.Continue
  def handleHuffmanCString(nodeAddr: Int,
                           currentStreamByte: Int, currentStreamBit: Int,
                           inBetween: Boolean): StreamStrState = {
    StreamStrState.Continue
  }
  def handleHuffmanUnicodeString(nodeAddr: Int,
                                 currentStreamByte: Int, currentStreamBit: Int,
                                 inBetween: Boolean): StreamStrState = {
    StreamStrState.Continue
  }
}

class GlkIOSystem(vm: GlulxVM, glk: Glk, rock: Int) extends IOSystem(vm, rock) {
  def id = 2
  def streamChar(c: Char)       { glk.put_char(c) }
  def streamUniChar(c: Int)     { glk.put_char_uni(c) }
  
  // streamstr actions
  def handleChar8(c: Char, inBetween: Boolean, currentStreamByte: Int,
                  currentStreamBit: Int) = {
    glk.put_char(c)
    StreamStrState.Continue
  }
  def handleChar32(c: Int, inBetween: Boolean, currentStreamByte: Int,
                   currentStreamBit: Int) = {
    glk.put_char_uni(c)
    StreamStrState.Continue
  }
  
  def handleHuffmanCString(nodeAddr: Int,
                           currentStreamByte: Int, currentStreamBit: Int,
                           inBetween: Boolean): StreamStrState = {
    var cstrAddr    = nodeAddr + 1
    var currentChar = vm.memByteAt(cstrAddr)
    while (currentChar != 0) {
      streamChar(currentChar.asInstanceOf[Char])
      cstrAddr    += 1
      currentChar = vm.memByteAt(cstrAddr)
    }
    StreamStrState.Continue
  }
  def handleHuffmanUnicodeString(nodeAddr: Int,
                                 currentStreamByte: Int, currentStreamBit: Int,
                                 inBetween: Boolean): StreamStrState = {
    var uniCStrAddr = nodeAddr + 1
    var currentChar = vm.memIntAt(uniCStrAddr)
    while (currentChar != 0) {
      streamChar((currentChar & 0xffff).asInstanceOf[Char])
      uniCStrAddr += 4
      currentChar = vm.memIntAt(uniCStrAddr)
    }
    StreamStrState.Continue
  }
}

class FilterIOSystem(vm: GlulxVM, functionAddr: Int)
extends IOSystem(vm, functionAddr) {
  def id = 1
  val funType = vm.state.memByteAt(functionAddr)
  def streamChar(c: Char) {
    vm.callWithArgs(DestTypes.DoNotStore, 0, vm.state.pc, vm.state.fp, functionAddr, c.toInt)
  }
  def streamUniChar(c: Int) {
    vm.callWithArgs(DestTypes.DoNotStore, 0, vm.state.pc, vm.state.fp, functionAddr, c)
  }

  override def streamNum(num: Int, pos: Int) {
    val numberString = "%d".format(num)
    if (pos == 0) {
      vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0, vm.state.pc, vm.state.fp)
    }
    
    if (pos >= numberString.length) {
      val fpVal    = vm.state.stack.popInt
      val pcVal    = vm.state.stack.popInt
      val destAddr = vm.state.stack.popInt
      val destType = vm.state.stack.popInt
      if (destType == DestTypes.ResumeExecuteFunction) {
        vm.state.pc = pcVal
        vm.state.fp = fpVal
      } else {
        throw new IllegalStateException("FALLBACK, SHOULD NOT HAPPEN, destType is: %d".format(destType))
      }
    } else {
      vm.state.pushCallStub(DestTypes.ResumePrintDecimal, pos + 1, num, vm.state.fp)
      vm.callWithArgsNoCallStub(functionAddr, numberString.charAt(pos).asInstanceOf[Int])
    }
  }

  override def handleResumeUniString(addr: Int):StreamStrState = {
    val currentChar = vm.state.memIntAt(addr)
    if (currentChar != 0) {
      vm.state.pushCallStub(DestTypes.ResumePrintUnicode, 0, addr + 4, vm.state.fp)
      vm.callWithArgsNoCallStub(functionAddr, currentChar)
      StreamStrState.CallFunction
    } else {
      StreamStrState.Finished
    }
  }
  override def handleResumeCString(addr: Int): StreamStrState = {
    val currentChar = vm.state.memByteAt(addr)
    if (currentChar != 0) {
      vm.state.pushCallStub(DestTypes.ResumePrintCString, 0, addr + 1, vm.state.fp)
      vm.callWithArgsNoCallStub(functionAddr, currentChar)
      StreamStrState.CallFunction
    } else {
      StreamStrState.Finished
    }
  }
  
  // streamstr actions
  private def suspendPrintCompressed(inBetween: Boolean,
                                     currentStreamByte: Int,
                                     currentStreamBit: Int) {
    if (!inBetween) {
      vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                            vm.state.pc, vm.state.fp)
    }
    vm.state.pushCallStub(DestTypes.ResumePrintCompressed, currentStreamBit,
                          currentStreamByte, vm.state.fp)
  }
  
  def handleChar8(c: Char, inBetween: Boolean, currentStreamByte: Int,
                  currentStreamBit: Int) = {
    suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit)
    vm.callWithArgsNoCallStub(functionAddr, c.asInstanceOf[Int])
    StreamStrState.CallFunction
  }
  def handleChar32(c: Int, inBetween: Boolean, currentStreamByte: Int,
                   currentStreamBit: Int) = {
    suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit)
    vm.callWithArgsNoCallStub(functionAddr, c.asInstanceOf[Int])
    StreamStrState.CallFunction
  }

  def handleHuffmanCString(nodeAddr: Int,
                           currentStreamByte: Int, currentStreamBit: Int,
                           inBetween: Boolean): StreamStrState = {
    suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit)
    StreamStrState.resumeCStringAt(nodeAddr + 1)
  }
  def handleHuffmanUnicodeString(nodeAddr: Int,
                                 currentStreamByte: Int, currentStreamBit: Int,
                                 inBetween: Boolean): StreamStrState = {
    suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit)
    StreamStrState.resumeUniStringAt(nodeAddr + 1)
  }

  override def handleStreamstrCString(inBetween: Boolean,
                                      stringAddress: Int) = {
    if (!inBetween) {
      vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                            vm.state.pc, vm.state.fp)
    }
    StreamStrState.resumeCStringAt(stringAddress + 1)
  }
  override def handleStreamstrUnicodeString(inBetween: Boolean,
                                            stringAddress: Int) = {
    if (!inBetween) {
      vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                            vm.state.pc, vm.state.fp)
    }
    StreamStrState.resumeUniStringAt(stringAddress + 4)
  }
}


