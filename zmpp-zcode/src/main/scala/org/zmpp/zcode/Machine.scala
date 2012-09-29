/*
 * Created on 2010/05/12
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
package org.zmpp.zcode

import org.zmpp.base.{Types, VMRunStates, Memory, CircularStack}
import java.util.StringTokenizer
import java.util.Random
import scala.annotation.switch

/**
 * This is the control part of the Z-Machine. Opposed to the first generation
 * ZMPP, this is a huge class. This design decision is mainly to keep everything
 * instruction execution related in one place and keep execution tight and
 * efficient.
 */
class Machine {
  val state                     = new VMStateImpl
  var ioSystem                  = new IoSystem(this)
  val readLineInfo              = new ReadLineInfo
  val readCharInfo              = new ReadCharInfo
  val randomGenerator           = new Random
  var screenModel : ScreenModel = null

  var objectTable: ObjectTable  = null  
  private[this] val undoSnapshots = new CircularStack[Snapshot](2)

  // for efficiency reasons, we cache the current decoding state here.
  // there is only one instance of decoding info for stage 1 and
  // one for each form to hold decoding information. This is, so weaker
  // JVM's like Dalvik has less objects to garbage collect
  private[this] val _decodeInfo = new DecodeInfo(0, 0, 0, 0)
  // Decode information end
  // transient information, current routine decoding data
  private[this] val _callArgs    = new Array[Int](8)
  private[this] var _currentArg  = 0
  private[this] var _iterations  = 1

  def init(story: Memory, screenModel: ScreenModel) {
    state.reset(story)
    objectTable = if (state.header.version <= 3) new ClassicObjectTable(this)
                  else new ModernObjectTable(this)
    this.screenModel = screenModel
    ioSystem.reset(screenModel)
    state.setCapabilityFlags(screenModel.capabilities ++ List(SupportsUndo))
  }
  def version = state.header.version 

  def resumeWithLineInput(input: String) {
    state.runState = ZMachineRunStates.Running
    val parserHelper =
      new ParserHelper(state, readLineInfo.textBuffer, readLineInfo.parseBuffer,
                       0, false)
    parserHelper.process(input)
    if (version >= 5) {
      if (input.length == 0) storeResult(0) // timed input cancelled
      else storeResult(10) // store terminator
    }
  }
  def resumeWithCharInput(c: Int) {
    state.runState = ZMachineRunStates.Running
    storeResult(state.encoding.unicodeToZSCII(c.asInstanceOf[Char]))
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
      "%d/%d".format(Types.signExtend16(global2), global3)
    } else {
      "%d:%02d".format(global2, global3)
    }
  }

  // **********************************************************************
  // ***** error/warning
  // *****************************
  def warn(msg: String) = ioSystem.printMessage("WARNING: %s".format(msg)) 
  def fatal(msg: String) {
    ioSystem.printMessage("FATAL: %s".format(msg))
    state.runState = VMRunStates.Halted
  }

  def readLine(text: Int, parse: Int, time: Int, routine: Int) = {
    readLineInfo.maxInputChars =
      if (state.header.version <= 4) state.byteAt(text) - 1
      else state.byteAt(text)
    readLineInfo.numLeftOverChars =
      if (state.header.version >= 5 && state.byteAt(text + 1) > 0) {
        state.byteAt(text + 1)
      } else 0

    readLineInfo.textBuffer  = text
    readLineInfo.parseBuffer = parse
    readLineInfo.time        = time
    readLineInfo.routine     = routine
    state.runState           = ZMachineRunStates.ReadLine
  }

  def readChar(time: Int, routine: Int) = {
    readCharInfo.time    = time
    readCharInfo.routine = routine
    state.runState       = ZMachineRunStates.ReadChar
  }
  // **********************************************************************
  // ***** Private methods
  // *****************************
  private def numOperands = _decodeInfo.numOperands
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

  private def nextOperand = {
    _currentArg += 1
    state.nextOperand(_decodeInfo.types(_currentArg - 1))
  }
  private def nextSignedOperand = Types.signExtend16(nextOperand)
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
  def callInterrupt(packedAddr: Int) {
    state.call(packedAddr, _callArgs, -1, 0)
  }
  private def callWithoutReturnValue(numCallArgs: Int) {
    val packedAddr = nextOperand
    var i = 0
    while (i < numCallArgs) {
      _callArgs(i) = nextOperand
      i += 1
    }
    state.call(packedAddr, _callArgs, -1, numCallArgs)
  }
  private def callWithReturnValue(numCallArgs: Int) {
    val packedAddr = nextOperand
    var i = 0
    while (i < numCallArgs) {
      _callArgs(i) = nextOperand
      i += 1
    }
    val storeVar = state.nextByte
    state.call(packedAddr, _callArgs, storeVar, numCallArgs)
  }
  private def callWithReturnValueVs2(numCallArgs: Int) {
    //printf("CALL_VS2 #ARGS = %d PC = %02x\n", numCallArgs, state.pc)
    val packedAddr = nextOperand
    var i = 0
    while (i < numCallArgs) {
      val argnum  = _currentArg - 1
      val vartype = _decodeInfo.types(argnum)
      _callArgs(i) = nextOperand
      //printf("ARG(%d) = %02x, TYPE = %d, ", argnum, _callArgs(i), vartype)
      i += 1
    }
    //println
    //printf("CALL_VS2 PC AFTER ARGS = %02x\n", state.pc)
    val storeVar = state.nextByte
    //printf("CALL_VS2 PC AFTER STOREVAR = %02x PC = %02x\n", storeVar, state.pc)
    state.call(packedAddr, _callArgs, storeVar, numCallArgs)
  }

  private def execute0Op {
    (_decodeInfo.opnum: @switch) match {
      case 0x00 => state.returnFromRoutine(1) // rtrue
      case 0x01 => state.returnFromRoutine(0) // rfalse
      case 0x02 => // print
        state.encoding.decodeZString(ioSystem)
      case 0x03 => // print_ret
        state.encoding.decodeZString(ioSystem)
        ioSystem.putChar('\n')
        state.returnFromRoutine(1)
      case 0x04 => // nop
      case 0x05 => // save V1-V4
        if (version <= 4) state.runState = ZMachineRunStates.SaveGame
        else fatal("illegal 0OP: $05, Version >= 5")
      case 0x06 => // restore V1-V4
        if (version <= 4) state.runState = ZMachineRunStates.RestoreGame
        else fatal("illegal 0OP: $06, Version >= 5")
      case 0x07 => // restart
        // bit 0 of flags2 (transcript)
        // bit 1 of flags2 (fixed pitch)
        val preserveFlags = state.byteAt(0x10) & 0x03
        state.restoreOriginalDynamicMem
        state.reset
        // restore preserved flags
        state.setByteAt(0x10, state.byteAt(0x10) | preserveFlags)
        screenModel.initUI
      case 0x08 => // ret_popped
        state.returnFromRoutine(state.variableValue(0))
      case 0x09 => // pop
        if (version < 5) {
          state.variableValue(0)
        } else { // V6 -> catch
          storeResult(state.fp)
        }
      case 0x0a => // quit
        ioSystem.printMessage("*Game Ended*")
        state.runState = VMRunStates.Halted
      case 0x0b => ioSystem.putChar('\n') // new_line
      case 0x0c => // show_status
        if (version > 3) fatal("@show_status not allowed in version > 3")
        else screenModel.updateStatusLine
      case 0x0d => // verify
        decideBranch(state.header.checksum == state.calculatedChecksum)
      case 0x0f => // piracy (as recommended in the spec, always branch)
        decideBranch(true)
      case _ =>
        fatal("illegal 0OP: $%02x".format(_decodeInfo.opnum))
    }
  }
  private def execute1Op {
    (_decodeInfo.opnum: @switch) match {
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
      case 0x07 => // print_addr
        state.encoding.decodeZStringAtByteAddress(nextOperand,
                                                  ioSystem)
      case 0x08 => // call_1s
        callWithReturnValue(0)
      case 0x09 => // remove_obj
        objectTable.removeObject(nextOperand)
      case 0x0a => // print_obj
        printObject(nextOperand, ioSystem)
      case 0x0b => state.returnFromRoutine(nextOperand) // ret
      case 0x0c => // jump
        val offset = nextSignedOperand
        state.pc += offset - 2 // address
      case 0x0d => // print_paddr
        state.encoding.decodeZStringAtPackedAddress(nextOperand,
                                                    ioSystem)
      case 0x0e => // load
        val varnum = nextOperand
        // see Std. 1.1
        val value = if (varnum == 0) state.stackTop
                    else state.variableValue(varnum)
        storeResult(value)
      case 0x0f => // 1-4 -> not, > 5 -> call_1n
        if (version <= 4) storeResult((~nextOperand) & 0xffff)
        else state.call(nextOperand, _callArgs, -1, 0)
      case _ =>
        fatal("illegal 1OP: $%02x".format(_decodeInfo.opnum))
    }
  }
  private def execute2Op {
    (_decodeInfo.opnum: @switch) match {
      case 0x01 => // je -> Note: Variable number of arguments !!!
        var equalsAny = false
        val first = nextOperand
        var i = 1
        while (i < numOperands) {
          if (nextOperand == first) equalsAny = true
          i += 1
        }
        decideBranch(equalsAny)
      case 0x02 => // jl
        decideBranch(nextSignedOperand < nextSignedOperand)
      case 0x03 => // jg
        decideBranch(nextSignedOperand > nextSignedOperand)
      case 0x04 => // dec_chk
        val varnum   = nextOperand
        val value    = nextSignedOperand
        val newValue = (Types.signExtend16(state.variableValue(varnum)) - 1)
          .asInstanceOf[Short]
        state.setVariableValue(varnum, newValue)
        decideBranch(newValue < value)
      case 0x05 => // inc_chk
        val varnum = nextOperand
        val value  = nextSignedOperand
        val newValue = (Types.signExtend16(state.variableValue(varnum)) + 1)
          .asInstanceOf[Short]
        state.setVariableValue(varnum, newValue)
        decideBranch(newValue > value)
      case 0x06 => // jin
        val obj1 = nextOperand
        val obj2 = nextOperand
        decideBranch(objectTable.parent(obj1) == obj2)
      case 0x07 => // test
        val op1 = nextOperand
        val op2 = nextOperand
        decideBranch((op1 & op2) == op2)
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
        val varnum = nextOperand
        val value = nextOperand
        if (varnum == 0) state.stack.pop // see Spec 1.1
        state.setVariableValue(varnum, value)
      case 0x0e => // insert_obj
        val obj  = nextOperand
        val dest = nextOperand
        //printf("insert_obj %d %d\n", obj, dest)
        objectTable.insertObject(obj, dest)
      case 0x0f => // loadw
        val array = nextOperand
        val wordIndex = nextOperand
        // note that values need to always be truncated to
        // unsigned 16 bit values !!!
        val memAddress = (array + wordIndex * 2) & 0xffff
        storeResult(state.shortAt(memAddress))
      case 0x10 => // loadb
        val array     = nextOperand
        val byteIndex = nextOperand
        // note that values need to always be truncated to
        // unsigned 16 bit values !!!
        val memAddress = (array + byteIndex) & 0xffff
        storeResult(state.byteAt(memAddress))
      case 0x11 => // get_prop
        storeResult(objectTable.propertyValue(nextOperand, nextOperand))
      case 0x12 => // get_prop_addr
        val obj = nextOperand
        val property = nextOperand
        if (obj > 0) {
          storeResult(objectTable.propertyAddress(obj, property) & 0xffff)
        } else {
          warn("@get_prop_addr illegal access to object " + obj)
          storeResult(0)
        }
      case 0x13 => // get_next_prop
        val obj = nextOperand
        val property = nextOperand
        if (obj > 0) {
          storeResult(objectTable.nextProperty(obj, property) & 0xffff)
        } else {
          warn("@get_next_prop illegal access to object " + obj)
          storeResult(0)
        }
      case 0x14 => // add
        storeResult((nextSignedOperand+nextSignedOperand).asInstanceOf[Short])
      case 0x15 => // sub
        storeResult((nextSignedOperand-nextSignedOperand).asInstanceOf[Short])
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
      case 0x1b => // set_colour
        val foreground = nextSignedOperand
        val background = nextSignedOperand
        val window = if (numOperands > 2) nextOperand
                     else ScreenModel.CurrentWindow
        screenModel.setColour(foreground, background, window)
      case 0x1c => // throw
        val returnValue = nextOperand
        state.unwindStackToFramePointer(nextOperand)
        state.returnFromRoutine(returnValue)
      case _ =>
        fatal("illegal 2OP: $%02x".format(_decodeInfo.opnum))
    }
  }
  private def executeVar {
    (_decodeInfo.opnum: @switch) match {
      case 0x00 => // call
        callWithReturnValue(numOperands - 1)
      case 0x01 => // storew
        val array     = nextOperand
        val wordIndex = nextSignedOperand
        // note that values need to always be truncated to
        // unsigned 16 bit values !!!
        val memAddress = (array + wordIndex * 2) & 0xffff
        val value     = nextOperand
        state.setShortAt(memAddress, value)
      case 0x02 => // storeb
        val array     = nextOperand
        val byteIndex = nextSignedOperand
        val value     = nextOperand
        // note that values need to always be truncated to
        // unsigned 16 bit values !!!
        val memAddress = (array + byteIndex) & 0xffff
        state.setByteAt(memAddress, value)
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
        val textBuffer = nextOperand
        val parseBuffer = if (numOperands > 1) nextOperand
                          else 0
        var time = 0
        var routine = 0
        if (version >= 4) {
          if (numOperands > 2) time = nextOperand
          if (numOperands > 3) routine = nextOperand
        }
        val terminator = readLine(textBuffer, parseBuffer, time, routine)
      case 0x05 => // print_char
        ioSystem.putChar(nextOperand.asInstanceOf[Char])
      case 0x06 => // print_num
        ioSystem.printNum(nextSignedOperand)
      case 0x07 => // random
        storeResult(random(nextSignedOperand))
      case 0x08 => // push
        state.setVariableValue(0, nextOperand)
      case 0x09 => // pull
        if (version == 6) {
          val userStack = if (numOperands > 0) nextOperand else 0
          if (userStack > 0) storeResult(state.popUserStack(userStack))
          else {
            if (state.stackEmpty) fatal("Stack underflow !")
            storeResult(state.variableValue(0))
          }
        } else {
          val varnum = nextOperand
          val value = state.variableValue(0)
          // standard 1.1: indirect access to variable 0 -> replace value in-place
          if (varnum == 0) state.replaceStackTopWith(value)
          else if (state.stackEmpty) fatal("Stack underflow !")
          else state.setVariableValue(varnum, value)
        }
      case 0x0a => // split_window
        if (screenModel != null) screenModel.splitWindow(nextOperand)
        else warn("@split_window, platformIO not set")
      case 0x0b => // set_window
        if (screenModel != null) screenModel.setWindow(nextOperand)
        else warn("@set_window, platformIO not set")
      case 0x0c => // call_vs2
        callWithReturnValueVs2(numOperands - 1)
      case 0x0d => // erase_window
        screenModel.eraseWindow(nextSignedOperand)
      case 0x0e => // erase_line
        screenModel.eraseLine(nextOperand)
      case 0x0f => // set_cursor
        val line   = nextOperand
        // column parameter is optional !
        val column = if (numOperands > 1) nextOperand else 1
        if (screenModel != null) screenModel.setCursorPosition(line, column)
        else warn("@set_window, platformIO not set")
      case 0x10 => // get_cursor
        val array     = nextOperand
        //printf("get_cursor $%02x\n", array)
        val (cursorRow: Int, cursorColumn: Int) = screenModel.cursorPosition
        state.setShortAt(array,     cursorRow)
        state.setShortAt(array + 2, cursorColumn)
      case 0x11 => // set_text_style
        screenModel.setTextStyle(nextOperand)
      case 0x12 => // buffer_mode
        screenModel.bufferMode(nextOperand)
      case 0x13 => // output_stream
        outputStream(nextSignedOperand)
      case 0x14 => // input_stream
        ioSystem.currentInputStreamId = nextOperand
      case 0x15 => // sound_effect
        val number = if (numOperands > 0) nextOperand else 1
        val effect = if (numOperands > 1) nextOperand else 2
        val volumeRepeats =
          if (numOperands > 2) nextOperand else 0x01a0
        val routine = if (numOperands > 3) nextOperand else 0
        //printf("TODO: @sound_effect not connected yet\n")
      case 0x16 => // readchar
        // first parameter: never used, always 1
        val inp = if (numOperands > 0) nextOperand else 1
        var time = 0
        var routine = 0
        if (version >= 4) {
          if (numOperands > 1) time = nextOperand
          if (numOperands > 2) routine = nextOperand
        }
        readChar(time, routine)
      case 0x17 => // scan_table
        val x     = nextOperand
        val table = nextOperand
        val len   = nextOperand
        val form = if (numOperands > 3) nextOperand else 0x82
        val result = scanTable(x, table, len, form)
        storeResult(result)
        decideBranch(result > 0)
      case 0x18 => // not (V5/V6)
        storeResult((~nextOperand) & 0xffff)
      case 0x19 => // call_vn
        callWithoutReturnValue(numOperands - 1)
      case 0x1a => // call_vn2
        callWithoutReturnValue(numOperands - 1)
      case 0x1b => // tokenise
        val textBuffer = nextOperand
        val parseBuffer = nextOperand
        val userDictionary = if (numOperands > 2) nextOperand else 0
        val flag = if (numOperands > 3) nextOperand else 0
        val parserHelper = new ParserHelper(state, textBuffer, parseBuffer,
                                            userDictionary, flag != 0)
        parserHelper.tokenize
      case 0x1c => // encode_text
        val zsciiText = nextOperand
        val length    = nextOperand
        val from      = nextOperand
        val codedText = nextOperand
        val encoder = new Encoder(state)
        val token = new Token(zsciiText + from,
                              zsciiText + from + length - 1)
        encoder.encode(token)
        var i = 0
        while (i < encoder.tokenBytes.length) {
          state.setByteAt(codedText + i, encoder.tokenBytes(i) & 0xff)
          i += 1
        }
      case 0x1d => // copy_table
        val first  = nextOperand
        val second = nextOperand
        val size   = nextSignedOperand
        copyTable(first, second, size)
      case 0x1e => // print_table
        if (numOperands < 2 || numOperands > 4) {
          fatal("@print_table wrong number of operands")
        }
        printTable(nextOperand, nextOperand,
                   if (numOperands > 2) nextOperand else 1,
                   if (numOperands > 3) nextOperand else 0)
      case 0x1f => // check_arg_count
        val argNum = nextOperand
        decideBranch(argNum <= state.numArgsCurrentRoutine)
      case _ =>
        fatal("illegal VAR: $%02x".format(_decodeInfo.opnum))
    }
  }
  private def executeExt {
    (_decodeInfo.opnum: @switch) match {
      case 0x00 => saveV5 // save
      case 0x01 => restoreV5 // restore
      case 0x02 => // log_shift
        val number: Char = nextOperand.asInstanceOf[Char]
        val places = nextSignedOperand
        val result = if (places < 0) number >>> -places
                     else number << places
        storeResult(result)
      case 0x03 => // art_shift
        val number: Short = nextSignedOperand.asInstanceOf[Short]
        val places = nextSignedOperand
        val result = if (places < 0) number >> -places
                     else number << places
        storeResult(result)
      case 0x04 => // set_font
        storeResult(screenModel.setFont(nextOperand))
      case 0x05 => // draw_picture
        fatal("@draw_picture not supported yet")
      case 0x06 => // picture_data
        fatal("@picture_data not supported yet")
      case 0x07 => // erase_picture
        fatal("@erase_picture not supported yet")
      case 0x08 => // set_margins
        fatal("@set_margins not supported yet")
      case 0x09 => // save_undo
        undoSnapshots.push(state.createSnapshot)
        storeResult(1)
      case 0x0a => // restore_undo
        if (undoSnapshots.empty) storeResult(0)
        else {
          state.readSnapshot(undoSnapshots.pop)
          storeResult(2)
        }
      case 0x0b => // print_unicode
        // printing a 16 bit char code means that the Z-Machine can only
        // handle characters from the BMP (Basic Multilingual Plane)
        ioSystem.putChar(nextOperand.asInstanceOf[Char])
      case 0x0c => // check_unicode
        // we simply assume that most Java systems can process Unicode
        nextOperand
        storeResult(3)
      case 0x10 => // move_window
        fatal("@move_window not supported yet")
      case 0x11 => // window_size
        fatal("@window_size not supported yet")
      case 0x12 => // window_style
        fatal("@window_style not supported yet")
      case 0x13 => // get_wind_prop
        fatal("@get_wind_prop not supported yet")
      case 0x14 => // scroll_window
        fatal("@scroll_window not supported yet")
      case 0x15 => // pop_stack
        val numItems = nextOperand
        val userStack = if (_decodeInfo.numOperands > 1) nextOperand else 0
        var i = 0
        while (i < numItems) {
          if (userStack == 0) state.variableValue(0)
          else state.popUserStack(userStack)
          i += 1
        }
      case 0x16 => // read_mouse
        fatal("@read_mouse not supported yet")
      case 0x17 => // mouse_window
        fatal("@mouse_window not supported yet")
      case 0x18 => // push_stack
        val value = nextOperand
        val userStack = if (_decodeInfo.numOperands > 1) nextOperand else 0
        val result = if (userStack == 0) {
          state.setVariableValue(0, value)
          decideBranch(true)
        } else decideBranch(state.pushUserStack(userStack, value))
      case 0x19 => // put_wind_prop
        fatal("@put_wind_prop not supported yet")
      case 0x1a => // print_form
        fatal("@print_form not supported yet")
      case 0x1b => // make_menu
        fatal("@make_menu not supported yet")
      case 0x1c => // picture_table (we do not need to implement this)
      case _ =>
        fatal("illegal EXT: $%02x".format(_decodeInfo.opnum))
    }
  }

  private def outputStream(streamnum: Int) {
    if (streamnum == 0) return
    else if (streamnum < 0) ioSystem.selectOutputStream(-streamnum, false)
    else if (streamnum == 3) {
      if (version == 6) {
        fatal("VERSION 6 NOT SUPPORTED YET")
      }
      ioSystem.selectMemoryStream(nextOperand)
    } else {
      ioSystem.selectOutputStream(streamnum, true)
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

  private def scanTable(x: Int, table: Int, len: Int, form: Int): Int = {
    val isWordType   = (form & 0x80) == 0x80
    val fieldLength = form & 0x7f
    var current = table
    var i = 0
    while (i < len) {
      val currentValue = if (isWordType) state.shortAt(current)
                         else state.byteAt(current)
      if (currentValue == x) return current
      current += fieldLength
      i += 1
    }
    0
  }

  private def copyTable(first: Int, second: Int, size: Int) {
    val absSize = if (size < 0) -size else size
    val secondStartsInFirst = second >= first && second < first + size
    val copyForward = size < 0 || !secondStartsInFirst
    if (second == 0) for (i <- 0 until absSize) state.setByteAt(first + i, 0)
    else if (copyForward) {
      for (i <- 0 until absSize)
        state.setByteAt(second + i, state.byteAt(first + i))
    } else {
      // copy backwards
      for (i <- (absSize - 1) to 0 by -1) {
        state.setByteAt(second + i, state.byteAt(first + i))
      }
    }
  }

  private def printTable(table: Int, width: Int, height: Int,
                         skip: Int) {
    //printf("@PRINT_TABLE, w = %d, h = %d, skip = %d\n",
    //       width, height, skip)
    var position = table
    var row = 0
    var col = 0
    val cursorPos = screenModel.cursorPosition

    while (row < height) {
      col = 0
      while (col < width) {
        ioSystem.putChar(state.encoding.zsciiToUnicode(
          state.byteAt(position).asInstanceOf[Char]), StreamIds.Screen)
        position += 1
        col += 1
      }
      row += 1
      screenModel.setCursorPosition(cursorPos._1 + row, cursorPos._2)
      position += skip
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
        // tricky: make sure we have maximum 4/8 parameters, yet have
        // the correct amount of operands stored in the decode info
        if (optype == OperandTypes.Omitted) hasMoreTypes = false
        else {
          index += 1
          hasMoreTypes = index < 4
        }
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
    (_decodeInfo.form: @switch) match {
      case 0  => decodeLongTypes  // FormLong
      case 1  => decodeShortTypes // FormShort
      case 2  => decodeVarTypes   // FormVar
      case 3  => decodeVarTypes   // FormExt
      case _ =>
        fatal("form not supported: %s\n".format(_decodeInfo.toString))
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
      operandCount = OperandCountExtVar
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

  def doInstruction(verbose: Boolean=false) {
    val oldpc = state.pc
    decodeInstruction
    decodeForm
    
/*
    // don't do this when in production - maybe a preprocessor should
    // be hooked in
    if (verbose) {
      printf("%04d - $%05x: %s %s\n", _iterations, oldpc,
             _decodeInfo.opcodeName(version), makeOperandString)
    }
*/
    // execute
    import Instruction._
    (_decodeInfo.operandCount: @switch) match {
      case 0  => execute0Op
      case 1  => execute1Op
      case 2  => execute2Op
      case -1 => executeVar // OperandCountVar
      case -2 => executeExt // OperandCountExt
      case _ => fatal("operand count not supported: %s\n".format(_decodeInfo.toString))
    }
    // don't do this when in production (preprocessor ?)
    //_iterations += 1
  }

  def setFontSizeInUnits(width: Int, height: Int) {
    if (version == 6) {
      state.setByteAt(0x26, height)
      state.setByteAt(0x27, width)
    } else if (version >= 5) { // 5, 7, 8
      state.setByteAt(0x26, width)
      state.setByteAt(0x27, height)
    }
  }

  def fontSizeInUnits: (Int, Int) = (state.header.fontWidthUnits, state.header.fontHeightUnits)

  def setScreenSizeInUnits(width: Int, height: Int) {
    if (version >= 4) {
      state.setByteAt(0x20, height)
      state.setByteAt(0x21, width)
    }
    if (version >= 5) {
      state.setShortAt(0x22, width)
      state.setShortAt(0x24, height)
    }
  }

  def screenSizeInUnits: (Int, Int) = (state.header.screenWidthUnits, state.header.screenHeightUnits)

  // **********************************************************************
  // ****** Save/restore games
  // **********************************************************************

  def resumeWithSaveStream(outputStream: java.io.OutputStream) {
    state.runState = ZMachineRunStates.Running
    val writer = new QuetzalWriter(state)
    val success = writer.write(outputStream)
    if (version <= 3) resumeSaveV3(success)
    else resumeSaveV4(success)
  }

  private def resumeSaveV3(success: Boolean) = decideBranch(success)
  private def resumeSaveV4(success: Boolean) = storeResult(if (success) 1 else 0)
  private def saveV5 {
    if (numOperands > 0) {
      // save data-area mode
      val table = nextOperand
      val numBytes = if (numOperands > 1) nextOperand else 0
      val name: String = if (numOperands > 2) {
        val nameAddress = nextOperand
        val nameLength = state.byteAt(nameAddress)
        val nameBuilder = new StringBuilder
        for (i <- 0 until nameLength) {
          nameBuilder.append(state.byteAt(nameAddress + 1 + i).asInstanceOf[Char])
        }
        nameBuilder.toString
      } else null

      if (table == 0 || numBytes == 0) {
        warn("save data area will fail, either table address or size is 0")
        storeResult(0)
      } else {
        warn("save data area not implemented yet")
        storeResult(0)
      }
    } else state.runState = ZMachineRunStates.SaveGame
  }

  def resumeWithRestoreStream(inputStream: java.io.InputStream) {
    state.runState = ZMachineRunStates.Running
    val reader = new QuetzalReader(state, this)
    val success = reader.read(inputStream)
    if (version <= 3) resumeRestoreV3(success)
    else resumeRestoreV4(success)
  }
  private def resumeRestoreV3(success: Boolean) = decideBranch(success)
  private def resumeRestoreV4(success: Boolean) = storeResult(if (success) 1 else 0)

  private def restoreV5 {
    if (numOperands > 0) {
      // save data-area mode
      val table = nextOperand
      val numBytes = if (numOperands > 1) nextOperand else 0
      val name: String = if (numOperands > 2) {
        val nameAddress = nextOperand
        val nameLength = state.byteAt(nameAddress)
        val nameBuilder = new StringBuilder
        for (i <- 0 until nameLength) {
          nameBuilder.append(state.byteAt(nameAddress + 1 + i).asInstanceOf[Char])
        }
        nameBuilder.toString
      } else null

      if (table == 0 || numBytes == 0) {
        warn("read data area will fail, either table address or size is 0")
        storeResult(0)
      } else {
        warn("read data area not implemented yet")
        storeResult(0)
      }
    } else state.runState = ZMachineRunStates.RestoreGame
  }
}
