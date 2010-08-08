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
import java.util.StringTokenizer
import java.util.Random

/**
 * This is the control part of the Z-Machine. Opposed to the first generation
 * ZMPP, this is a huge class. This design decision is mainly to keep everything
 * instruction execution related in one place and keep execution tight and
 * efficient.
 */
class Machine {
  val state                   = new VMState
  var ioSystem                = new IoSystem
  val readLineInfo            = new ReadLineInfo
  val randomGenerator         = new Random

  def currentOutputStream  = ioSystem.currentOutputStream
  //def keyboardStream       = _platformIO.keyboardStream

  var objectTable: ObjectTable = null

  // for efficiency reasons, we cache the current decoding state here.
  // there is only one instance of decoding info for stage 1 and
  // one for each form to hold decoding information. This is, so weaker
  // JVM's like Dalvik has less objects to garbage collect
  private val _decodeInfo = new DecodeInfo(0, 0, 0, 0)
  // Decode information end
  // transient information, current routine decoding data
  private val _callArgs    = new Array[Int](8)
  private var _currentArg  = 0

  var iterations  = 1

  def init(story: Memory, platformIO: PlatformIO) {
    state.reset(story)
    objectTable = if (state.header.version <= 3) new ClassicObjectTable(this)
                  else new ModernObjectTable(this)
    ioSystem.reset(platformIO)
  }

  def version = state.header.version
  
  def doTurn {
    while (state.runState == VMRunStates.Running) {
      executeInstruction
    }
  }

  // ***********************************************************************
  // ******** PARSER SUPPORT
  // **************************************  
  //private def textBufferOffset = if (version < 5) 1 else 2

  def resumeWithLineInput(input: String) {
    state.runState = VMRunStates.Running
    val parserSupport =
      new ParserHelper(state, readLineInfo.textBuffer, readLineInfo.parseBuffer,
                       0, false)
    parserSupport.process(input)
  }

  // ***********************************************************************
  // ******** EVERYTHING ELSE
  // **************************************  
  // Status (V1-V3)
  def statusLineObjectName: String = {
    val bufferStream = new StringBuilderOutputStream
    printObject(state.variableValue(0x10), bufferStream)
    bufferStream.toString
  }
  
  def statusLineScoreOrTime: String = {
    val global2 = state.variableValue(0x11)
    val global3 = state.variableValue(0x12)
    if (state.header.isScoreGame) {
      "%d/%d".format(signExtend16(global2), global3)
    } else {
      "%d:%d".format(global2, global3)
    }
  }

  // **********************************************************************
  // ***** error/warning
  // *****************************
  def warn(msg: String) {
    printf("WARNING: %s", msg)
  }
 
  def fatal(msg: String) {
    printf("FATAL: %s", msg)
    state.runState = VMRunStates.Halted
  }

  def readLine(text: Int, parse: Int) = {
    ioSystem.flush
    readLineInfo.maxInputChars =
      if (state.header.version <= 4) state.byteAt(text) - 1
      else state.byteAt(text)
    readLineInfo.textBuffer  = text
    readLineInfo.parseBuffer = parse
    state.runState = VMRunStates.WaitForEvent
  }
  // **********************************************************************
  // ***** Private methods
  // *****************************
  
  private def printObject(obj: Int, outputStream: OutputStream) {
    state.encoding.decodeZStringAtByteAddress(
      objectTable.propertyTableAddress(obj) + 1, outputStream)
  }

  private def makeOperandString = {
    val builder = new StringBuilder
    var operandAddress = state.pc
    for (i <- 0 until _decodeInfo.numOperands) {
      if (i > 0) builder.append(", ")
      val optype = _decodeInfo.types(i)
      if (optype == OperandTypes.LargeConstant) {
        builder.append("$%02x".format(state.shortAt(operandAddress)))
        operandAddress += 2
      } else if (optype == OperandTypes.SmallConstant) {
        builder.append("$%02x".format(state.byteAt(operandAddress)))
        operandAddress += 1
      } else if (optype == OperandTypes.Variable) {
        val varnum = state.byteAt(operandAddress)
        val varname = if (varnum == 0) "(SP)"
                      else if (varnum <= 0x0f) "L%02x".format(varnum - 1)
                      else if (varnum >= 0x10) "G%02x".format(varnum - 0x10)
                      else "???"
        val varvalue = if (varnum == 0) state.stackTop
                       else state.variableValue(varnum)
        builder.append("%s[#$%02x]".format(varname, varvalue))
        operandAddress += 1
      }
    }
    builder.toString
  }
  
  private def signExtend8(value: Int) = {
    if ((value & 0x80) == 0x80) value | 0xffffff00 else value
  }
  private def signExtend16(value: Int) = {
    if ((value & 0x8000) == 0x8000) value | 0xffff0000 else value
  }

  private def nextOperand = {
    _currentArg += 1
    state.nextOperand(_decodeInfo.types(_currentArg - 1))
  }
  private def nextSignedOperand = signExtend16(nextOperand)
  private def storeResult(result: Int) = state.setVariableValue(state.nextByte,
                                                                result)

  private def doBranch(branchOffset: Int) {
    if (branchOffset == 0)      state.returnFromRoutine(0)
    else if (branchOffset == 1) state.returnFromRoutine(1)
    else                        state.pc += branchOffset - 2
  }
  private def decideBranch(cond: Boolean) {
    val branchByte0 = state.nextByte
    val branchOnTrue = ((branchByte0 & 0x80) == 0x80)
    val branchOffset = if ((branchByte0 & 0x40) == 0x40) branchByte0 & 0x3f
      else {
        val branchByte1 = state.nextByte
        val branchOffsetVal = ((branchByte0 & 0x3f) << 8) | branchByte1

        // 14 bit sign extend
        if ((branchOffsetVal & 0x2000) == 0x2000) branchOffsetVal | 0xffffc000
        else branchOffsetVal
      }
    if (branchOnTrue && cond || !branchOnTrue && !cond) doBranch(branchOffset)
  }
  private def callWithoutReturnValue(numCallArgs: Int) {
    val packedAddr = nextOperand
    for (i <- 0 until numCallArgs) {
      _callArgs(i) = nextOperand
    }
    state.call(packedAddr, _callArgs, -1, numCallArgs)
  }
  private def callWithReturnValue(numCallArgs: Int) {
    val packedAddr = nextOperand
    for (i <- 0 until numCallArgs) {
      _callArgs(i) = nextOperand
    }
    val storeVar = state.nextByte
    state.call(packedAddr, _callArgs, storeVar, numCallArgs)
  }
  private def callWithReturnValueVs2(numCallArgs: Int) {
    printf("CALL_VS2 #ARGS = %d PC = %02x\n", numCallArgs, state.pc)
    val packedAddr = nextOperand
    for (i <- 0 until numCallArgs) {
      val argnum  = _currentArg - 1
      val vartype = _decodeInfo.types(argnum)
      _callArgs(i) = nextOperand
      printf("ARG(%d) = %02x, TYPE = %d, ", argnum, _callArgs(i), vartype)
    }
    println
    printf("CALL_VS2 PC AFTER ARGS = %02x\n", state.pc)
    val storeVar = state.nextByte
    printf("CALL_VS2 PC AFTER STOREVAR = %02x PC = %02x\n", storeVar, state.pc)
    state.call(packedAddr, _callArgs, storeVar, numCallArgs)
  }

  private def execute0Op {
    _decodeInfo.opnum match {
      case 0x00 => state.returnFromRoutine(1) // rtrue
      case 0x01 => state.returnFromRoutine(0) // rfalse
      case 0x02 => // print
        state.encoding.decodeZString(currentOutputStream)
      case 0x03 => // print_ret
        state.encoding.decodeZString(currentOutputStream)
        ioSystem.putChar('\n')
        state.returnFromRoutine(1)
      case 0x04 => // nop
      case 0x08 => // ret_popped
        state.returnFromRoutine(state.variableValue(0))
      case 0x0b => currentOutputStream.putChar('\n') // new_line
      case _ =>
        throw new UnsupportedOperationException(
          "0OP opnum: 0x%02x\n".format(_decodeInfo.opnum))
    }
  }
  private def execute1Op {
    _decodeInfo.opnum match {
      case 0x00 => decideBranch(nextOperand == 0) // jz
      case 0x01 => // get_sibling
        val sibling = objectTable.sibling(nextOperand)
        storeResult(sibling)
        decideBranch(sibling != 0)
      case 0x02 => // get_child
        val child = objectTable.child(nextOperand)
        storeResult(child)
        decideBranch(child != 0)
      case 0x03 => // get_parent
        storeResult(objectTable.parent(nextOperand))
      case 0x04 => // get_prop_len
        storeResult(objectTable.propertyLength(nextOperand))
      case 0x05 => // inc
        val varnum = nextOperand
        state.setVariableValue(varnum, state.variableValue(varnum) + 1)
      case 0x06 => // dec
        val varnum = nextOperand
        state.setVariableValue(varnum,
                               (state.variableValue(varnum) - 1) & 0xffff)
      case 0x08 => // call_1s
        callWithReturnValue(0)
      case 0x0a => // print_obj
        printObject(nextOperand, currentOutputStream)
      case 0x0b => state.returnFromRoutine(nextOperand) // ret
      case 0x0c => // jump
        val offset = nextSignedOperand
        state.pc += offset - 2 // address
      case 0x0d => // print_paddr
        state.encoding.decodeZStringAtPackedAddress(nextOperand,
                                                    currentOutputStream)
      case 0x0f => // 1-4 -> not, > 5 -> call_1n
        if (version <= 4) storeResult((~nextOperand) & 0xffff)
        else state.call(nextOperand, _callArgs, -1, 0)
      case _ =>
        throw new UnsupportedOperationException(
          "1OP opnum: 0x%02x\n".format(_decodeInfo.opnum))
    }
  }
  private def execute2Op {
    _decodeInfo.opnum match {
      case 0x01 => // je -> Note: Variable number of arguments !!!
        var equalsAny = false
        val first = nextOperand
        for (i <- 1 until _decodeInfo.numOperands) {
          if (nextOperand == first) equalsAny = true
        }
        decideBranch(equalsAny)
      case 0x02 => // jl
        decideBranch(nextSignedOperand < nextSignedOperand)
      case 0x03 => // jg
        decideBranch(nextSignedOperand > nextSignedOperand)
      case 0x04 => // dec_chk
        val varnum   = nextOperand
        val value    = nextSignedOperand
        val newValue = signExtend16(state.variableValue(varnum)) - 1
        state.setVariableValue(varnum, newValue)
        decideBranch(newValue < value)
      case 0x05 => // inc_chk
        val varnum = nextOperand
        val value  = nextSignedOperand
        val newValue = signExtend16(state.variableValue(varnum)) + 1
        state.setVariableValue(varnum, newValue)
        decideBranch(newValue > value)
      case 0x06 => // jin
        val obj1 = nextOperand
        val obj2 = nextOperand
        decideBranch(objectTable.parent(obj1) == obj2)
      case 0x08 => // or
        storeResult(nextOperand | nextOperand)
      case 0x09 => // and
        storeResult(nextOperand & nextOperand)
      case 0x0a => // test_attr
        val obj  = nextOperand
        val attr = nextOperand
        decideBranch(objectTable.isAttributeSet(obj, attr))
      case 0x0b => // set_attr
        objectTable.setAttribute(nextOperand, nextOperand)
      case 0x0c => // clear_attr
        objectTable.clearAttribute(nextOperand, nextOperand)
      case 0x0d => // store
        state.setVariableValue(nextOperand, nextOperand)
      case 0x0e => // insert_obj
        val obj  = nextOperand
        val dest = nextOperand
        //printf("insert_obj %d %d\n", obj, dest)
        objectTable.insertObject(obj, dest)
      case 0x0f => // loadw
        val array = nextOperand
        val wordIndex = nextOperand
        storeResult(state.shortAt(array + (wordIndex << 1)))
      case 0x10 => // loadb
        val array     = nextOperand
        val byteIndex = nextOperand
        storeResult(state.byteAt(array + byteIndex))
      case 0x11 => // get_prop
        storeResult(objectTable.propertyValue(nextOperand, nextOperand))
      case 0x12 => // get_prop_addr
        val obj = nextOperand
        val property = nextOperand
        if (obj > 0) {
          storeResult(objectTable.propertyAddress(obj, property) & 0xffff)
        } else {
          warn("@get_prop_addr illegal access to object " + obj)
        }
      case 0x14 => // add
        storeResult(nextSignedOperand + nextSignedOperand)
      case 0x15 => // sub
        storeResult(nextSignedOperand - nextSignedOperand)
      case 0x16 => // mul
        storeResult(nextSignedOperand * nextSignedOperand)
      case 0x17 => // div
        val op1 = nextSignedOperand
        val op2 = nextSignedOperand
        if (op2 == 0) fatal("@div division by zero")
        else storeResult(op1 / op2)
      case 0x18 => // mod
        val op1 = nextSignedOperand
        val op2 = nextSignedOperand
        if (op2 == 0) fatal("@mod division by zero")
        else storeResult(op1 % op2)
      case 0x19 => // call_2s
        callWithReturnValue(1)
      case 0x1a => // call_2n
        callWithoutReturnValue(1)
      case _ =>
        throw new UnsupportedOperationException(
          "2OP opnum: 0x%02x\n".format(_decodeInfo.opnum))
    }
  }
  private def executeVar {
    _decodeInfo.opnum match {
      case 0x00 => // call
        callWithReturnValue(_decodeInfo.numOperands - 1)
      case 0x01 => // storew
        val array     = nextOperand
        val wordIndex = nextOperand
        val value     = nextOperand
        //printf("storew $%02x %d %d\n", array, wordIndex, value)
        state.setShortAt(array + (wordIndex << 1), value)
      case 0x02 => // storeb
        val array     = nextOperand
        val byteIndex = nextOperand
        val value     = nextOperand
        state.setByteAt(array + byteIndex, value)
      case 0x03 => // put_prop
        val obj      = nextOperand
        val property = nextOperand
        val value    = nextOperand
        if (obj > 0) {
          try {
            objectTable.setPropertyValue(obj, property, value)
          } catch {
            case noprop: PropertyDoesNotExistException =>
              fatal("Property %d of Object %d does not exist.".format(obj,
                                                                      property))
            case e: Exception =>
              fatal("Unknown Exception: %s".format(e.getMessage))
          }
        } else {
          warn("@put_prop illegal access to object %d".format(obj))
        }
      case 0x04 => // sread V1-V3
        val terminator = readLine(nextOperand, nextOperand)
      case 0x05 => // print_char
        currentOutputStream.putChar(nextOperand.asInstanceOf[Char])
      case 0x06 => // print_num
        ioSystem.printNum(nextSignedOperand)
      case 0x07 => // random
        storeResult(random(nextSignedOperand))
      case 0x08 => // push
        state.setVariableValue(0, nextOperand)
      case 0x09 => // pull
        if (state.stackEmpty) fatal("Stack underflow !")
        else storeResult(state.variableValue(0))
      case 0x0c => // call_vs2
        callWithReturnValueVs2(_decodeInfo.numOperands - 1)
      case 0x11 => // set_text_style
        val style = nextOperand
        printf("@set_text_style %d not implemented yet\n", style)
      case 0x19 => // call_vn
        callWithoutReturnValue(_decodeInfo.numOperands - 1)
      case 0x1a => // call_vn2
        callWithoutReturnValue(_decodeInfo.numOperands - 1)
      case 0x1f => // check_arg_count
        val argNum = nextOperand
        decideBranch(argNum <= state.numArgsCurrentRoutine)
      case _ =>
        throw new UnsupportedOperationException(
          "VAR opcode not supported: 0x%02x\n".format(_decodeInfo.opnum))
    }
  }

  private def random(range: Int): Int = {
    if (range < 0) {
      randomGenerator.setSeed(-range)
      0
    } else if (range == 0) {
      randomGenerator.setSeed(System.currentTimeMillis)
      0
    } else {
      // nextInt(range) returns [0, range[, but we need [1, range]
      randomGenerator.nextInt(range) + 1
    }
  }

  private def decodeVarTypes {
    var typeByte = state.nextByte
    var hasMoreTypes = true
    var index = 0
    var offset = 0 // offset for more than 4 parameters (call_vs2/call_vn2)

    while (hasMoreTypes) {
      // Type extraction is a little complicated, because we can have up
      // to two type bytes.
      // Th type bytes are arranged in this way:
      // 00112233, so we shift like this
      // typeByte >> 6 & 0x03
      // typeByte >> 4 & 0x03
      // typeByte >> 2 & 0x03
      // typeByte >> 0 & 0x03
      val optype = (typeByte >> (6 - ((index & 0x03) << 1))) & 0x03
      _decodeInfo.types(index + offset) = optype

      // one of CALL_VN2/CALL_VS2, there is an additional type byte
      if (_decodeInfo.isCallVx2 && index == 3 && offset == 0) {
        index = 0
        typeByte = state.nextByte
        offset = 4
      } else {
        if (optype == OperandTypes.Omitted || index == 4) hasMoreTypes = false
        else index += 1
      }
    }
    _decodeInfo.numOperands = index + offset
  }
  private def decodeLongTypes {
    _decodeInfo.numOperands = 2
    _decodeInfo.types(0) =
      if ((_decodeInfo.opcode & 0x40) == 0x40) OperandTypes.Variable
      else OperandTypes.SmallConstant
    _decodeInfo.types(1) =
      if ((_decodeInfo.opcode & 0x20) == 0x20) OperandTypes.Variable
      else OperandTypes.SmallConstant
  }
  private def decodeShortTypes {
    val optype = _decodeInfo.opcode & 0x30
    if (optype == 0x30) _decodeInfo.numOperands = 0
    else {
      _decodeInfo.numOperands = 1
      _decodeInfo.types(0) = optype >> 4
    }
  }

  private def decodeForm {
    _decodeInfo.form match {
      case Instruction.FormShort => decodeShortTypes
      case Instruction.FormLong  => decodeLongTypes
      case Instruction.FormVar   => decodeVarTypes
      case Instruction.FormExt   => decodeVarTypes
      case _ =>
        throw new UnsupportedOperationException(
          "form not supported: %s\n".format(_decodeInfo.toString))
    }
  }

  private def decodeInstruction {
    val byte0        = state.nextByte
    var form         = 0
    var operandCount = 0
    var opcode    = 0

    import Instruction._
    if (byte0 == 0xbe) {
      form         = FormExt
      operandCount = OperandCountVar
      opcode       = state.nextByte
    } else {
      val formSel  = byte0 & 0xc0
      if (formSel == 0xc0) {
        form         = FormVar
        operandCount = if ((byte0 & 0x20) == 0x20) OperandCountVar else 2
        opcode       = byte0 & 0x1f
      } else if (formSel == 0x80) {
        form         = FormShort
        operandCount = if ((byte0 & 0x30) == 0x30) 0 else 1
        opcode       = byte0 & 0x0f
      } else {
        form         = FormLong
        operandCount = 2
        opcode       = byte0 & 0x1f
      }
    }
    _decodeInfo.set(form, operandCount, opcode, byte0)
    _currentArg = 0
  }

  private def executeInstruction {
    val oldpc = state.pc
    decodeInstruction
    decodeForm
    printf("%04d - $%05x: %s %s\n", iterations, oldpc,
           _decodeInfo.opcodeName(version), makeOperandString)
    // execute
    import Instruction._
    _decodeInfo.operandCount match {
      case 0 => execute0Op
      case 1 => execute1Op
      case 2 => execute2Op
      case OperandCountVar => executeVar
      case _ =>
        throw new UnsupportedOperationException(
          "form not supported: %s\n".format(_decodeInfo.toString))
    }
    iterations += 1
  }  
}

