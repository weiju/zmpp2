/*
 * Created on 2010/04/01
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
import java.util.logging._

import org.zmpp.base._

// ***************************************************************************
// ****
// **** VM state
// ****
// ***************************************************************************

/**
 * GlulxVMState captures the internal state of the Glulx system:
 * - story memory
 * - RAM access
 * - stack access
 *  - local variables
 *  - functions
 */
class GlulxVMState extends VMState {
  val logger = Logger.getLogger("glulx")
  private[this] var _story    : Memory = null
  private[this] var _header   : GlulxStoryHeader = null
  
  // Memory setup
  private[this] var _memheap  : MemoryHeap = null
  private[this] var _extMem   : Memory = null
  private[this] var _extEnd   : Int = 0
  private[this] var _extstart : Int = 0 // cached, because accessed frequently
  var runState = VMRunStates.Running
  private[this] var stack     : Stack = null
  // Registers
  var pc                      = 0
  var fp                      = 0
  def sp                      = stack.sp
  def sp_=(newpointer: Int)   = stack.sp_=(newpointer)
  def init(story: Memory) {
    _story     = story
    _header    = new GlulxStoryHeader(story)
    stack      = new Stack(_header.stacksize)
    _extstart  = _header.extstart
    _memheap   = new MemoryHeap(_header.endmem)
    pc         = 0
    fp         = 0
    runState   = VMRunStates.Running
    memsize    = header.endmem
    logger.info("VM INITIALIZED WITH EXT_START: %d END_MEM: %d".format(
                _extstart, header.endmem))
  }
  def restart(originalRam: Array[Byte],
              protectionStart: Int, protectionLength: Int) {
    logger.info("@restart (start: %02x, # bytes: %02x)".format(
                protectionStart, protectionLength))
    logger.info("HEAP ACTIVE: %b EXT_START: $%02x EXT_END: $%02x".format(
                _memheap.active, _extstart, _extEnd))
    stack      = new Stack(_header.stacksize)
    _memheap   = new MemoryHeap(_header.endmem)
    memsize    = header.endmem
    pc         = 0
    fp         = 0
    runState   = VMRunStates.Running
    
    // reset bytes in ram
    val ramsize = _extstart - _header.ramstart
    protectedMemRestore(originalRam, 0, _header.ramstart, ramsize,
                        protectionStart, protectionLength)
  }
  
  private def protectedMemRestore(memarray: Array[Byte], srcOffset: Int,
                                  destOffset: Int, numBytes: Int,
                                  protectionStart: Int,
                                  protectionLength: Int) {
    if (protectionLength > 0) {
      for (i <- 0 until numBytes) {
        val destAddress = destOffset + i
        if (destAddress < protectionStart ||
            destAddress >= protectionStart + protectionLength) {
          _story.setByteAt(destAddress, memarray(i))
        }
      }
    } else {
      _story.copyBytesFrom(memarray, srcOffset, destOffset, numBytes)
    }
  }

  private def _setExtendedMem(size: Int) {
    val currentExtSize = if (_extMem == null) 0 else _extEnd - _extstart
    // note: we actually do not need to "shrink" extended memory, we only
    // need to extend it
    if (currentExtSize != size) {
      if (size > currentExtSize) {
        val extbytes = new Array[Byte](size)
        if (_extMem != null) {
          _extMem.copyBytesTo(extbytes, _extstart, currentExtSize)
        }
        _extMem = new DefaultMemory(extbytes, _extstart)
      }
    }
    _extEnd = _extstart + size
  }

  def story        = _story
  def header       = _header
  def heapIsActive = _memheap.active
  def ramSize      = memsize - header.ramstart

  private def inStoryMem(addr: Int) = addr < _extstart
  private def inExtMem(addr: Int) =
    addr >= _extstart && addr < _extEnd
  private def fitsInStoryMem(addr: Int, size: Int) = {
    inStoryMem(addr) && inStoryMem(addr + size - 1)
  }
  private def fitsOnHeap(addr: Int, size: Int) =
    _memheap.memblockAt(addr) != null

  // Stack interface
  def empty            = stack.empty
  def localsPos        = stack.getInt(fp + Stack.OffsetLocalsPos)
  def frameLen         = stack.getInt(fp)
  def cloneStackValues = stack.cloneValues
  def initStackFromByteArray(arr: Array[Byte]) = stack.initFromByteArray(arr)

  def pushByte(value : Int)  = stack.pushByte(value)
  def topByte : Int          = stack.topByte
  def popByte : Int          = {
    if (sp <= fp + frameLen)
      throw new IllegalStateException("POP BYTE - STACK UNDERFLOW !!")
    stack.popByte
  }
  def pushShort(value: Int)  = stack.pushShort(value)
  def topShort : Int         = stack.topShort
  def popShort : Int         = {
    if (sp <= fp + frameLen)
      throw new IllegalStateException("POP SHORT - STACK UNDERFLOW !!")
    stack.popShort
  }
  def pushInt(value : Int)   = stack.pushInt(value)
  def topInt : Int           = stack.topInt
  def popIntUnchecked        = stack.popInt
  def popInt : Int           = {
    if (sp <= fp + frameLen)
      throw new IllegalStateException("POP INT - STACK UNDERFLOW !!")
    stack.popInt
  }

  def setIntInStack(addr: Int, value: Int)  = stack.setInt(addr, value)
  def getIntInStack(addr: Int)              = stack.getInt(addr)
  def setByteInStack(addr: Int, value: Int) = stack.setByte(addr, value)
  def getByteInStack(addr: Int)             = stack.getByte(addr)
  def numStackValuesInCallFrame = (sp - (fp + frameLen)) / 4
  
  // special stack functions
  def stackSwap {
    if (numStackValuesInCallFrame < 2)
      throw new IllegalStateException("STACK SWAP - NOT ENOUGH STACK VALUES")
    val val0 = stack.popInt
    val val1 = stack.popInt
    stack.pushInt(val0)
    stack.pushInt(val1)
  }
  def stackPeek(i: Int): Int = {
    if (numStackValuesInCallFrame <= i) {
      throw new IllegalStateException("STACK PEEK - NOT ENOUGH STACK VALUES")
    }
    stack.getInt(sp - (4 * (i + 1)))
  }
  def stackRoll(numValues: Int, numRotatePlaces: Int) {
    if (numRotatePlaces == 0) return
    val tmparr = new Array[Int](numValues)
    var i = 0
    while (i < numValues) {
      var pos = ((numValues - 1 - i) + numRotatePlaces) % numValues
      if (pos < 0) pos = numValues + pos
      tmparr(pos) = stack.popInt
      i += 1
    }
    i = 0
    while (i < numValues) {
      stack.pushInt(tmparr(i))
      i += 1
    }
  }

  // Memory interface
  def memByteAt    (addr: Int) : Int = {
    if      (inStoryMem(addr)) _story.byteAt(addr)
    else if (inExtMem(addr))   _extMem.byteAt(addr)
    else                       _memheap.byteAt(addr)
  }
  def setMemByteAt (addr: Int, value: Int) {
    if (addr < header.ramstart) {
      logger.warning("SETTING BYTE VALUE IN ROM %02x = %d !".format(addr,
                                                                    value))
    }
    if      (inStoryMem(addr)) _story.setByteAt(addr, value)
    else if (inExtMem(addr))   _extMem.setByteAt(addr, value)
    else                       _memheap.setByteAt(addr, value)
  }
  def memShortAt   (addr: Int) : Int = {
    if      (inStoryMem(addr)) _story.shortAt(addr)
    else if (inExtMem(addr))   _extMem.shortAt(addr)
    else                       _memheap.shortAt(addr)
  }
  def setMemShortAt(addr: Int, value: Int) {
    if (addr < header.ramstart) {
      logger.warning("SETTING SHORT VALUE IN ROM %02x = %d !".format(addr,
                                                                     value))
    }
    if      (inStoryMem(addr)) _story.setShortAt(addr, value)
    else if (inExtMem(addr))   _extMem.setShortAt(addr, value) 
    else                       _memheap.setShortAt(addr, value)
  }
  def memIntAt     (addr: Int) : Int = {
    if      (inStoryMem(addr)) _story.intAt(addr)
    else if (inExtMem(addr))   _extMem.intAt(addr)
    else                       _memheap.intAt(addr)
  }
  def setMemIntAt  (addr: Int, value: Int) {
    if (addr < header.ramstart) {
      logger.warning("SETTING INT VALUE IN ROM %02x = %d !".format(addr,
                                                                   value))
    }
    if      (inStoryMem(addr)) _story.setIntAt(addr, value)
    else if (inExtMem(addr))   _extMem.setIntAt(addr, value)
    else                       _memheap.setIntAt(addr, value)
  }

  def ramByteAt    (address : Int) : Int =
    memByteAt(header.ramstart + address)
  def setRamByteAt (address : Int, value : Int) =
    setMemByteAt(header.ramstart + address, value)
  def ramShortAt   (address : Int) : Int =
    memShortAt(header.ramstart + address)
  def setRamShortAt(address : Int, value : Int) =
    setMemShortAt(header.ramstart + address, value)
  def ramIntAt     (address : Int) : Int = memIntAt(header.ramstart + address)
  def setRamIntAt  (address : Int, value : Int) =
    setMemIntAt(header.ramstart + address, value)

  def malloc(size: Int) = _memheap.allocate(size)
  def mfree(addr: Int)  = _memheap.free(addr)
  
  def mcopy(numBytes: Int, srcAddr: Int, destAddr: Int) {
    if (fitsInStoryMem(srcAddr, numBytes) &&
        fitsInStoryMem(destAddr, numBytes)) {
      _story.copyBytesTo(destAddr, srcAddr, numBytes)
    }
    else if (fitsOnHeap(srcAddr, numBytes) && fitsOnHeap(destAddr, numBytes)) {
      _memheap.copyBytesTo(destAddr, srcAddr, numBytes)
    } else {
      for (i <- 0 until numBytes) {
        setMemByteAt(destAddr + i, memByteAt(srcAddr + i))
      }
    }
  }
  def mzero(numBytes: Int, addr: Int) {
    for (i <- 0 until numBytes) setMemByteAt(addr + i, 0)
  }
  def memsize   = if (_memheap.active) _memheap.maxAddress else _extEnd
  def memsize_=(newSize: Int) = _setExtendedMem(newSize - _extstart)
  def heapStart = if (_memheap.active) _extEnd else 0

  // Pushes a call stub, given an operand
  def pushCallStub(storeLocation : Operand) {
    pushCallStub(DestTypes.fromAddressMode(storeLocation.addressMode),
                 storeLocation.value, pc, fp)
  }
  def pushCallStub(destType: Int, destAddr: Int) {
    pushCallStub(destType, destAddr, pc, fp)
  }

  // generic call stub pushing, can take other values for pc and fp
  def pushCallStub(destType: Int, destAddr: Int, pcVal: Int, fpVal: Int) {
    stack.pushInt(destType)
    stack.pushInt(destAddr)
    stack.pushInt(pcVal)
    stack.pushInt(fpVal)
  }
  
  def popCallStubThrow(retval: Int) {
    fp           = stack.popInt
    pc           = stack.popInt
    val destAddr = stack.popInt
    val destType = stack.popInt
    storeResult(destType, destAddr, retval)
  }

  private def setValueInStack(index: Int, vartype: Int, value: Int) {
    (vartype: @switch) match {
      case 1 => // Types.ByteType
        stack.setByte(index, value)
      case 2 => // Types.ShortType
        stack.setShort(index, value)
      case 4 => // Types.IntType
        stack.setInt(index, value)
      case _ =>
        throw new IllegalStateException("unknown local type: " + vartype)
    }
  }
  
  def storeResult(destType: Int, destAddress: Int, value: Int) {
    (destType: @switch) match {
      case 0 => // DestTypes.DoNotStore, do nothing
      case 1 => setMemIntAt(destAddress, value) // DestTypes.Memory
      case 2 => setLocalAtAddress(destAddress, value) // DestTypes.LocalVariable
      case 3 => pushInt(value) // DestTypes.Stack
      case 4 => setRamIntAt(destAddress, value) // DestTypes.Ram
      case _ => 
        throw new IllegalArgumentException(
          "unsupported dest type for store: " + destType)
    }
  }

  // ***********************************************************************
  // ***** Local variable access
  // *********************************
  // determine the current index of the specified local variable relative
  // to the current frame pointer. By returning values, relative to the current
  // frame, we facilitate debugging  by only needing to look at the current
  // frame.
  // we do this by searching the format of the locals and adding pad counts
  // if necessary
  private def alignAddress(address : Int, datatype: Int) : Int = {
    // set to multiple of 4 for ltype == 4 and to even for ltype == 2
    if (datatype == Types.IntType && ((address & 0x03) != 0)) {
      address + (Types.SizeInt - (address & 0x03));
    } else if (datatype == Types.ShortType && ((address & 0x01) != 0)) {
      address + Types.SizeByte;
    } else {
      address
    }
  }

  /**
   * Stores an int at the specified address within the current frame's local
   * space. Note that we do not check the type, simply set the value without
   * a check with type Int. A couple of games seem to rely on the interpreter
   * letting them to write to locals that do not exist.
   */
  def setLocalAtAddress(destAddr: Int, value: Int) {
    stack.setInt(fp + localsPos + destAddr, value)
  }
  /**
   * Analogous to setLocalAtAddress(), this returns an int-sized value
   * and does not check the format, as the Glulx specification requests.
   */
  def getLocalAtAddress(localAddr : Int): Int = {
    stack.getInt(fp + localsPos + localAddr)
  }
  
  // For copyb/copys
  def getLocalByteAtAddress(localAddr : Int): Int = {
    stack.getByte(fp + localsPos + localAddr)
  }
  def getLocalShortAtAddress(localAddr : Int): Int = {
    stack.getShort(fp + localsPos + localAddr)
  }
  def setLocalByteAtAddress(destAddr: Int, value: Int) {
    stack.setByte(fp + localsPos + destAddr, value)
  }
  def setLocalShortAtAddress(destAddr: Int, value: Int) {
    stack.setShort(fp + localsPos + destAddr, value)
  }

  // ***********************************************************************
  // ***** Access local variables through variable numbers
  // *************************************************************

  private def localFrameIndex(localNum: Int) : Int = {
    var descriptorPos    = fp + Stack.OffsetLocalsFormat
    var currentLocalPos  = localsPos
    var localRangeStart    = 0
    var hasMoreDescriptors = true

    while (hasMoreDescriptors) {
      val ltype    = stack.getByte(descriptorPos)
      val nlocals  = stack.getByte(descriptorPos + 1)
      // needs alignment of localPos ?
      descriptorPos = alignAddress(descriptorPos, ltype)
      // the variable is within the current range, determine which one
      // and returns its address
      if (localRangeStart + nlocals > localNum) {
        return currentLocalPos + (localNum - localRangeStart) * ltype
      }
      // adjust variables
      hasMoreDescriptors = ltype != 0 && nlocals != 0
      currentLocalPos    += ltype * nlocals // advance to next variable block
      localRangeStart    += nlocals         // advance range index
      // advance to next descriptor pair
      descriptorPos      += GlulxVM.SizeLocalDescriptor
    }
    throw new IllegalStateException("unknown local variable: " + localNum)
  }

  private def localType(localNum: Int) : Int = {
    var descriptorPos = fp + Stack.OffsetLocalsFormat
    var localRangeStart    = 0
    var hasMoreDescriptors = true

    while (hasMoreDescriptors) {
      val ltype    = stack.getByte(descriptorPos)
      val nlocals  = stack.getByte(descriptorPos + 1)
      if (localRangeStart + nlocals > localNum) return ltype
      hasMoreDescriptors = ltype != 0 && nlocals != 0

      // advance to next descriptor pair
      descriptorPos   += GlulxVM.SizeLocalDescriptor
      localRangeStart += nlocals
    }
    0
  }

  /**
   * Sets a specific local variable.
   */
  def setLocal(localNum : Int, value : Int) {
    val ltype  = localType(localNum)
    if (ltype != 0) {
      val lindex = localFrameIndex(localNum)
      // Note: Only set a local if it exists !!!
      setValueInStack(fp + lindex, ltype, value)
    }
  }

  // do not implement if for now  
  def verify: Int = 0

  override def toString = {
    val builder = new StringBuilder
    builder.append("pc = $%02x stackframe = $%02x\n".format(pc, fp))
    builder.append(stack.toStringFrom(fp))
    builder.toString
  }
  
  def stackValuesAsString = {
    val builder = new StringBuilder
    val stackStart = fp + frameLen
    val numElems = numStackValuesInCallFrame
    builder.append("[")
    for (i <- 0 until numElems) {
      if (i > 0) builder.append(", ")
      builder.append("#$%02x".format(stack.getInt(stackStart + (i * 4))))
    }
    builder.append("]")
    builder.toString
  }
  
  // Reading data at the PC
  def nextByte: Int  = {
    pc += 1
    memByteAt(pc - 1)
  }
  def nextShort: Int = {
    pc += 2
    memShortAt(pc - 2)
  }
  def nextInt: Int   = {
    pc += 4
    memIntAt(pc - 4)
  }

  // ***********************************************************************
  // ***** State Serialization
  // *************************************************************
  def readSnapshot(snapshot: Snapshot, protectionStart: Int,
                   protectionLength: Int) {
    val ramsize = _extstart - header.ramstart
    protectedMemRestore(snapshot.ram, 0, _header.ramstart, ramsize,
                        protectionStart, protectionLength)
    stack.initFromByteArray(snapshot.stack)
    
    // TODO: Extmem and Heap
  }
  
  def createSnapshot(storeLocation: Operand): Snapshot = {
    val ram         = cloneRam
    logger.info("CREATE_SNAPSHOT, PC = $%02x FP = %d SP: %d".format(pc,
                                                                    fp, sp))
    pushCallStub(storeLocation)
    val stackValues = stack.cloneValues
    val extmem: Array[Byte] = null

    // TODO: extmem and heap
    new Snapshot(ram, stackValues, extmem)
  }

  def cloneRam = {
    val ramsize = _extstart - header.ramstart
    logger.info("Copying %d Bytes of RAM to preserve initial data".format(ramsize))
    val ram = new Array[Byte](ramsize)
    _story.copyBytesTo(ram, header.ramstart, ramsize)
    ram
  }  
}
