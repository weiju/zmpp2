/*
 * Created on 2010/05/12
 * Copyright (c) 2010, Wei-ju Wu.
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
package org.zmpp.zcode

import org.zmpp.base.VMRunStates
import org.zmpp.base.Memory

class StoryHeader(story: Memory) {
  def version             = story.byteAt(0x00)
  def flags1              = story.byteAt(0x01)
  def himemStart          = story.shortAt(0x04)
  def startPC             = story.shortAt(0x06)
  def dictionary          = story.shortAt(0x08)
  def objectTable         = story.shortAt(0x0a)
  def globalVars          = story.shortAt(0x0c)
  def staticStart         = story.shortAt(0x0e)
  def flags2              = story.shortAt(0x10)
  def abbrevTable         = story.shortAt(0x18)
  def fileLength          = story.shortAt(0x1a)
  def checksum            = story.shortAt(0x1c)
  def terpNumber          = story.shortAt(0x1e) // V4
  def terpVersion         = story.shortAt(0x1f) // V4
  def screenHeight        = story.byteAt(0x20) // V4
  def screenWidth         = story.byteAt(0x21) // V4
  def screenWidthUnits    = story.shortAt(0x22) // V5
  def screenHeightUnits   = story.shortAt(0x24) // V5
  def fontWidthUnits      = {
    if (version == 6) story.shortAt(0x27)
    else story.shortAt(0x26)
  }
  def fontHeightUnits     = {
    if (version == 6) story.shortAt(0x26)
    else story.shortAt(0x27)
  }
  def routinesOffset      = story.shortAt(0x28)
  def staticStringsOffset = story.shortAt(0x2a)
  def defaultBackground   = story.shortAt(0x2c)
  def defaultForeground   = story.shortAt(0x2e)
  def pixelWidthInStream3_=(width: Int) = story.setShortAt(0x30, width)
  def standardRevision_=(revision: Int) = story.setShortAt(0x32, revision)
  def alphabetTable       = story.shortAt(0x34)
  def headerExtTable      = story.shortAt(0x36)
  
  def unpackRoutineAddress(addr: Int) = {
    version match {
      case 1 => addr << 1
      case 2 => addr << 1
      case 3 => addr << 1
      case 4 => addr << 2
      case 5 => (addr << 2) + (routinesOffset << 3)
      case 6 => (addr << 2) + (routinesOffset << 3)
      case 8 => addr << 3
    }
  }
  def unpackStringAddress(addr: Int) = {
    version match {
      case 1 => addr << 1
      case 2 => addr << 1
      case 3 => addr << 1
      case 4 => addr << 2
      case 5 => (addr << 2) + (staticStringsOffset << 3)
      case 6 => (addr << 2) + (staticStringsOffset << 3)
      case 8 => addr << 3
    }
  }
  
  def isScoreGame = if (version < 3) true else (flags1 & 0x02) == 0
}

// cheap stack implementation. This stack holds int's, but the only int
// value that might get stored is the return address in the call frame
// (only happens on a push).
class Stack {
  private val _values = new Array[Int](100) // 100 for now

  var sp = 0
  
  def push(value: Int) {
    _values(sp) = value
    sp += 1
  }
  def pop = {
    sp -= 1
    _values(sp) & 0xffff
  }
  def top = _values(sp - 1)
  // there is only one single case where we need this function: return
  // PCs.
  def value32At(index: Int) = _values(index)
  def valueAt(index: Int) = _values(index) & 0xffff // truncate to 16 bit
  def setValueAt(index: Int, value: Int) = _values(index) = value
  override def toString = {
    val builder = new StringBuilder
    for (i <- 0 until sp) {
      builder.append("%d ".format(_values(i)))
    }
    builder.toString
  }
}

object FrameOffset {
  val ReturnPC = 0
  val OldFP    = 1
  val StoreVar = 2
  val NumArgs  = 3
  val Locals   = 4
}
class VMState {
  private var _story : Memory = null
  private val _stack = new Stack

  var header     : StoryHeader   = null
  var encoding = new ZsciiEncoding(this)
  var runState = VMRunStates.Running

  var pc       = 0
  var fp       = 0 // frame pointer
  def sp       = _stack.sp

  def reset(story: Memory) {
    _story = story
    header = new StoryHeader(_story)
    if (header.version != 6) {
      pc = header.startPC
    }
    encoding.reset
  }
  def byteAt(addr: Int)  = _story.byteAt(addr)
  def shortAt(addr: Int) = _story.shortAt(addr)
  def intAt(addr: Int)   = _story.intAt(addr)
  def setByteAt(addr: Int, value: Int)  = _story.setByteAt(addr, value)
  def setShortAt(addr: Int, value: Int) {
    if (addr == 0) throw new IllegalArgumentException("AAAAAH")
    _story.setShortAt(addr, value)
  }
  def setIntAt(addr: Int, value: Int)   = _story.setIntAt(addr, value)
  
  def nextByte = {
    pc += 1
    _story.byteAt(pc - 1)
  }
  def nextShort = {
    pc += 2
    _story.shortAt(pc - 2)
  }
  
  def variableValue(varnum: Int) = {
    if (varnum == 0) _stack.pop
    else if (varnum >= 1 && varnum <= 15) {
      // local
      //printf("Access Local L%02x\n", (varnum - 1))
      _stack.valueAt(fp + FrameOffset.Locals + (varnum - 1))
    } else {
      // global
      //printf("Access global: G%02x\n", varnum - 0x10)
      _story.shortAt(header.globalVars + ((varnum - 0x10) << 1))
    }
  }
  def setVariableValue(varnum: Int, value: Int) {
    if (varnum == 0) _stack.push(value)
    else if (varnum >= 1 && varnum <= 15) {
      // local
      //printf("Write local: L%02x = %d\n", varnum - 1, value)
      _stack.setValueAt(fp + FrameOffset.Locals + (varnum - 1), value)
    } else if (varnum >= 16) {
      // global
      _story.setShortAt(header.globalVars + ((varnum - 0x10) << 1), value)
    }
    // => throw away varnums < 0
  }

  def nextOperand(operandType: Int) = {
    import OperandTypes._
    operandType match {
      case LargeConstant =>
        pc += 2
        _story.shortAt(pc - 2)
      case SmallConstant =>
        pc += 1
        _story.byteAt(pc - 1)
      case Variable      =>
        pc += 1
        variableValue(_story.byteAt(pc - 1))
      case _ =>
        throw new UnsupportedOperationException(
          "unknown operand type: " + operandType)
    }
  }

  def call(packedAddr: Int, args: Array[Int], storeVar: Int, numArgs: Int) {
    //printf("CALL PACKED ADDR: $%02x storevar: %d\n", packedAddr, storeVar)
    if (packedAddr == 0) setVariableValue(storeVar, 0)
    else {
      val routineAddr = header.unpackRoutineAddress(packedAddr)
      val numLocals = _story.byteAt(routineAddr)
      
      //debug
      /*
      val builder = new StringBuilder
      builder.append("call $%02x ".format(routineAddr))
      for (i <- 0 until numArgs) {
        builder.append("#$%02x ".format(args(i)))
      }
      builder.append(" -> %d".format(storeVar))
      printf("%s\n", builder.toString)*/
      //debug
    
      // create a call frame
      val oldfp = fp
      fp = sp // current frame pointer
      _stack.push(pc) // return address
      _stack.push(oldfp)
      _stack.push(storeVar)
      _stack.push(numArgs)
      pc = routineAddr + 1 // place PC after routine header
      if (header.version <= 4) {
        for (i <- 0 until numLocals) _stack.push(nextShort)
      } else {
        for (i <- 0 until numLocals) _stack.push(0)
      }
      // set arguments to locals, throw away excess parameters (6.4.4.1)
      val numParams = if (numArgs <= numLocals) numArgs else numLocals
      for (i <- 0 until numParams) {
        _stack.setValueAt(fp + FrameOffset.Locals + i, args(i))
      }
      //printf("# locals: %d PC IS: $%02x\n", numLocals, pc)
    }
  }
  def returnFromRoutine(retval: Int) {
    val retpc    = _stack.value32At(fp + FrameOffset.ReturnPC)
    val oldfp    = _stack.valueAt(fp + FrameOffset.OldFP)
    val storeVar = _stack.valueAt(fp + FrameOffset.StoreVar)
    _stack.sp = fp
    fp = oldfp
    pc = retpc
    //printf("RET #$%02x -> VAR[%d]\n", retval, storeVar)
    setVariableValue(storeVar, retval)
  }

  def numArgsCurrentRoutine = _stack.valueAt(fp + FrameOffset.NumArgs)
}

object Instruction {
  val FormLong        = 0
  val FormShort       = 1
  val FormVar         = 2
  val FormExt         = 3
  val OperandCountVar = -1
}

object OperandTypes {
  val LargeConstant = 0x00
  val SmallConstant = 0x01
  val Variable      = 0x02
  val Omitted       = 0x03
  
  def typeName(optype: Int) = optype match {
    case LargeConstant => "LargeConstant"
    case SmallConstant => "SmallConstant"
    case Variable      => "Variable"
    case Omitted       => "Omitted"
    case _             => "???"
  }
}

class DecodeInfo(var form: Int, var operandCount: Int, var opnum: Int,
                 var opcode: Int) {
  val types = new Array[Int](8)
  var numOperands = 0

  def set(f: Int, oc: Int, opn: Int, b0: Int) = {
    form         = f
    operandCount = oc
    opnum        = opn
    opcode       = b0
    this
  }
  override def toString = {
    "[%s, %s, OPNUM: 0x%02x, OPCODE: 0x%02x]".format(formName, opCount, opnum, opcode)
  }
  private def formName = {
    import Instruction._
    form match {
      case FormLong  => "Long"
      case FormShort => "Short"
      case FormVar   => "Var"
      case FormExt   => "Ext"
      case _         => "???"
    }
  }
  private def opCount = {
    if (operandCount == Instruction.OperandCountVar) "Var"
    else "%dOP".format(operandCount)
  }
}

trait OutputStream {
  def printChar(c: Char)
  def printNum(num: Int)
  def flush
}

trait InputStream {
  def readLine: Int
}

trait PlatformIO {
  def screenOutputStream: OutputStream
  def keyboardStream: InputStream
}

class StringBuilderOutputStream extends OutputStream {
  val builder = new StringBuilder
  def printChar(c: Char) = builder.append(c)
  def printNum(num: Int) = builder.append(num)
  def flush {}
  override def toString = builder.toString
}

class ReadLineInfo {
  var textBuffer: Int    = 0
  var parseBuffer: Int   = 0
  var maxInputChars: Int = 0
}

