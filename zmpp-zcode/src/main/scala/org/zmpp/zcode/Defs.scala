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

import org.zmpp.base.VMRunStates
import org.zmpp.base.Memory
import org.zmpp.iff.QuetzalCompression

sealed class CapabilityFlag
case object SupportsColors      extends CapabilityFlag
case object SupportsUndo        extends CapabilityFlag
case object SupportsBoldFont    extends CapabilityFlag
case object SupportsItalicFont  extends CapabilityFlag
case object SupportsFixedFont   extends CapabilityFlag
case object SupportsTimedInput  extends CapabilityFlag
case object SupportsSound       extends CapabilityFlag
case object SupportsScreenSplit extends CapabilityFlag
case object SupportsMouse       extends CapabilityFlag
case object SupportsMenus       extends CapabilityFlag
case object SupportsPictures    extends CapabilityFlag

class StoryHeader(story: Memory) {
  def version             = story.byteAt(0x00)
  def flags1              = story.byteAt(0x01)
  def releaseNumber       = story.shortAt(0x02)
  def himemStart          = story.shortAt(0x04)
  def startPC             = story.shortAt(0x06)
  def dictionary          = story.shortAt(0x08)
  def objectTable         = story.shortAt(0x0a)
  def globalVars          = story.shortAt(0x0c)
  def staticStart         = story.shortAt(0x0e)
  def flags2              = story.shortAt(0x10)
  def serialNumber        = {
    val result = new Array[Byte](6)
    for (i <- 0 until 6) result(i) = story.byteAt(0x12 + i).asInstanceOf[Byte]
    result
  }
  def abbrevTable         = story.shortAt(0x18)
  def fileLength          = {
    val factor = if (version <= 3) 2
                 else if (version == 4 || version == 5) 4
                 else 8
    story.shortAt(0x1a) * factor
  }
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
      case 5 => addr << 2
      case 6 => (addr << 2) + (routinesOffset << 3)
      case 7 => (addr << 2) + (routinesOffset << 3)
      case 8 => addr << 3
    }
  }
  def unpackStringAddress(addr: Int) = {
    version match {
      case 1 => addr << 1
      case 2 => addr << 1
      case 3 => addr << 1
      case 4 => addr << 2
      case 5 => addr << 2
      case 6 => (addr << 2) + (staticStringsOffset << 3)
      case 7 => (addr << 2) + (staticStringsOffset << 3)
      case 8 => addr << 3
    }
  }
  
  def isScoreGame = if (version < 3) true else (flags1 & 0x02) == 0
}

// cheap stack implementation. This stack holds int's, but the only int
// value that might get stored is the return address in the call frame
// (only happens on a push).
class Stack {
  private val _values = new Array[Int](1024)
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
  def replaceTopWith(value: Int) {
    _values(sp - 1) = value
  }

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

  def cloneValues : Array[Int] = {
    val values = new Array[Int](sp)
    for (i <- 0 until sp) values(i) = _values(i)
    values
  }

  def initFromArray(values: Array[Int]) {
    for (i <- 0 until values.length) _values(i) = values(i)
    sp = values.length
  }
}

object FrameOffset {
  val ReturnPC     = 0
  val OldFP        = 1
  val StoreVar     = 2
  val NumArgs      = 3
  val NumLocals    = 4
  val Locals       = 5
  val NumInfoWords = 5
}

object ZMachineRunStates {
  val Halted       = VMRunStates.Halted
  val Running      = VMRunStates.Running
  val ReadLine     = VMRunStates.WaitForEvent
  val ReadChar     = 11
  val SaveGame     = 12
  val RestoreGame  = 13
}

/**
 * An undo snapshot of the vm state, the dynamic memory is compressed
 * using the same method as Quetzal to reduce memory footprint
 */
class Snapshot(val compressedDiff: Array[Byte], val stackValues: Array[Int],
               val pc: Int, val fp: Int)

trait VMState {
  def header: StoryHeader
  def encoding: ZsciiEncoding
  def runState: Int
  def pc: Int
  def pc_=(newpc: Int)

  def byteAt(addr: Int): Int
  def shortAt(addr: Int): Int
  def intAt(addr: Int): Int
  def setByteAt(addr: Int, value: Int)
  def setShortAt(addr: Int, value: Int)
  def setIntAt(addr: Int, value: Int)
}

class VMStateImpl extends VMState {
  import QuetzalCompression._

  private var _story : Memory = null
  private val _stack = new Stack

  var header     : StoryHeader   = null
  val encoding = new ZsciiEncoding(this)
  var runState = VMRunStates.Running
  var calculatedChecksum = 0
  // store the original dynamic memory as a reference point for
  // restart, undo snapshots and saving
  var originalDynamicMem: Array[Byte] = null

  var pc       = 0
  var fp       = 0 // frame pointer
  def sp       = _stack.sp
  def stack    = _stack

  def storyData = _story.buffer

  def reset {
    _stack.sp = 0
    // Set the initial frame pointer to -1. This is serving as a marker
    // when we search the stack to save
    fp        =  -1
    if (header.version != 6) {
      pc = header.startPC      
    } else {
      // V6 does function call to main routine
      call(header.startPC, null, -1, 0)
    }
    encoding.reset
    // set interpreter information
    setByteAt(0x1e, 0x06)
    setByteAt(0x1f, '6'.asInstanceOf[Int])
    setShortAt(0x32, 0x0101)    
  }

  def reset(story: Memory) {
    _story = story
    header    = new StoryHeader(_story)
    saveOriginalDynamicMem

    // calculate the checksum before we make any in-memory modifications
    // but after we have a header
    calculatedChecksum = calculateChecksum
    reset
  }

  private def saveOriginalDynamicMem {
    val dynamicMemSize = header.staticStart
    originalDynamicMem = new Array[Byte](dynamicMemSize)
    System.arraycopy(storyData, 0, originalDynamicMem, 0, dynamicMemSize)
  }
  def restoreOriginalDynamicMem {
    System.arraycopy(originalDynamicMem, 0, storyData, 0, header.staticStart)
  }

  private def calculateChecksum = {
    var currentByteAddress = 0x40
    var checksum = 0
    printf("CALC checksum, file size: %d, stored file size: %d\n",
           _story.size, header.fileLength)
    while (currentByteAddress < header.fileLength) {
      checksum += byteAt(currentByteAddress)
      currentByteAddress += 1
    }
    checksum & 0xffff
  }

  def byteAt(addr: Int)  = _story.byteAt(addr)
  def shortAt(addr: Int) = _story.shortAt(addr)
  def intAt(addr: Int)   = _story.intAt(addr)
  def setByteAt(addr: Int, value: Int)  = {
    if (addr >= header.staticStart) {
      throw new IllegalArgumentException("Attempt to write to static memory.")
    }
    _story.setByteAt(addr, value)
  }
  def setShortAt(addr: Int, value: Int) {
    if (addr >= header.staticStart) {
      throw new IllegalArgumentException("Attempt to write to static memory.")
    }
    _story.setShortAt(addr, value)
  }
  def setIntAt(addr: Int, value: Int)   = {
    if (addr >= header.staticStart) {
      throw new IllegalArgumentException("Attempt to write to static memory.")
    }
    _story.setIntAt(addr, value)
  }
  
  def nextByte = {
    pc += 1
    _story.byteAt(pc - 1)
  }
  def nextShort = {
    pc += 2
    _story.shortAt(pc - 2)
  }
  def stackEmpty = _stack.sp == 0
  def stackTop = _stack.top
  def replaceStackTopWith(value: Int) = _stack.replaceTopWith(value)
  def pushUserStack(userStack: Int, value: Int): Boolean = {
    val capacity = _story.shortAt(userStack)
    if (capacity == 0) false
    else {
      _story.setShortAt(userStack + capacity * 2, value)
      _story.setShortAt(userStack, capacity - 1)
      true
    }
  }
  def popUserStack(userStack: Int): Int = {
    val capacity = _story.shortAt(userStack)
    _story.setShortAt(userStack, capacity + 1)
    _story.shortAt(userStack + (capacity + 1) * 2)
  }

  def variableValue(varnum: Int) = {
    if (varnum == 0) _stack.pop
    else if (varnum >= 1 && varnum <= 15) { // local
      _stack.valueAt(fp + FrameOffset.Locals + (varnum - 1))
    } else { // global
      _story.shortAt(header.globalVars + ((varnum - 0x10) << 1))
    }
  }
  def setVariableValue(varnum: Int, value: Int) {
    if (varnum == 0) _stack.push(value)
    else if (varnum >= 1 && varnum <= 15) { // local
      //printf("SET L%02x TO %02x\n", varnum, value)
      _stack.setValueAt(fp + FrameOffset.Locals + (varnum - 1), value)
    } else if (varnum >= 16 && varnum <= 255) { // global
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
    if (packedAddr == 0) setVariableValue(storeVar, 0)
    else {
      val routineAddr = header.unpackRoutineAddress(packedAddr)
      val numLocals = _story.byteAt(routineAddr)
    
      // create a call frame
      val oldfp = fp
      fp = sp // current frame pointer
      _stack.push(pc) // return address
      _stack.push(oldfp)
      _stack.push(storeVar)
      _stack.push(numArgs)
      _stack.push(numLocals)
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
    }
  }
  def returnFromRoutine(retval: Int) {
    val retpc    = _stack.value32At(fp + FrameOffset.ReturnPC)
    val oldfp    = _stack.valueAt(fp + FrameOffset.OldFP)
    val storeVar = _stack.valueAt(fp + FrameOffset.StoreVar)
    _stack.sp = fp
    fp = oldfp
    pc = retpc
    setVariableValue(storeVar, retval)
  }

  def unwindStackToFramePointer(targetFramePointer: Int) {
    while (fp != targetFramePointer) {
      val oldfp = _stack.valueAt(fp + FrameOffset.OldFP)
      _stack.sp = fp
      fp = oldfp
    }
  }

  def numArgsCurrentRoutine = _stack.valueAt(fp + FrameOffset.NumArgs)
  def createSnapshot : Snapshot = {
    new Snapshot(compressDiffBytes(storyData, originalDynamicMem,
                                   header.staticStart),
                 _stack.cloneValues, pc, fp)
  }

  def readSnapshot(snapshot: Snapshot) {
    decompressDiffBytes(snapshot.compressedDiff, originalDynamicMem,
                        storyData, header.staticStart)
    _stack.initFromArray(snapshot.stackValues)
    pc = snapshot.pc
    fp = snapshot.fp
  }

  def setCapabilityFlags(flags: List[CapabilityFlag]) {
    var flags1 = byteAt(0x01)
    var flags2 = byteAt(0x10)
    flags.foreach(flag => flag match {
      case SupportsColors      => if (header.version >= 5) flags1 |= 0x01
      case SupportsPictures    => if (header.version == 6) flags1 |= 0x02
      case SupportsBoldFont    => if (header.version >= 4) flags1 |= 0x04
      case SupportsItalicFont  => if (header.version >= 4) flags1 |= 0x08
      case SupportsFixedFont   => if (header.version >= 4) flags1 |= 0x10
      case SupportsScreenSplit => if (header.version != 6) flags1 |= 0x20
      case SupportsSound       => if (header.version == 6) flags1 |= 0x20
      case SupportsTimedInput  => if (header.version >= 4) flags1 |= 0x80
      case _ => // do nothing
    })
    setByteAt(0x01, flags1)
    setByteAt(0x10, flags2)
  }
}

object Instruction {
  val FormLong           = 0
  val FormShort          = 1
  val FormVar            = 2
  val FormExt            = 3
  val OperandCountVar    = -1
  val OperandCountExtVar = -2
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
    opcodeName(5)
  }
  private def formName = {
    form match {
      case Instruction.FormLong  => "Long"
      case Instruction.FormShort => "Short"
      case Instruction.FormVar   => "Var"
      case Instruction.FormExt   => "Ext"
      case _         => "???"
    }
  }
  private def opCount = {
    if (operandCount == Instruction.OperandCountVar) "Var"
    else "%dOP".format(operandCount)
  }
  def isCallVx2 = {
    operandCount == Instruction.OperandCountVar &&
    (opnum == 0x1a || opnum == 0x0c)
  }

  def opcodeName(version: Int) = {
    operandCount match {
      case 0 => Oc0Op.opcodeName(opnum, version)
      case 1 => Oc1Op.opcodeName(opnum, version)
      case 2 => Oc2Op.opcodeName(opnum, version)
      case Instruction.OperandCountVar    => OcVar.opcodeName(opnum, version)
      case Instruction.OperandCountExtVar => OcExt.opcodeName(opnum, version)
      case _         => "???"
    }
  }
}

class ReadLineInfo {
  var textBuffer: Int    = 0
  var parseBuffer: Int   = 0
  var maxInputChars: Int = 0
}

object Oc2Op extends Enumeration {
  val Je          = Value(0x01, "JE")
  val Jl          = Value(0x02, "JL")
  val Jg          = Value(0x03, "JG")
  val DecChk      = Value(0x04, "DEC_CHK")
  val IncChk      = Value(0x05, "INC_CHK")
  val Jin         = Value(0x06, "JIN")
  val Test        = Value(0x07, "TEST")
  val Or          = Value(0x08, "OR")
  val And         = Value(0x09, "AND")
  val TestAttr    = Value(0x0a, "TEST_ATTR")
  val SetAttr     = Value(0x0b, "SET_ATTR")
  val ClearAttr   = Value(0x0c, "CLEAR_ATTR")
  val Store       = Value(0x0d, "STORE")
  val InsertObj   = Value(0x0e, "INSERT_OBJ")
  val Loadw       = Value(0x0f, "LOADW")
  val Loadb       = Value(0x10, "LOADB")
  val GetProp     = Value(0x11, "GET_PROP")
  val GetPropAddr = Value(0x12, "GET_PROP_ADDR")
  val GetNextProp = Value(0x13, "GET_NEXT_PROP")
  val Add         = Value(0x14, "ADD")
  val Sub         = Value(0x15, "SUB")
  val Mul         = Value(0x16, "MUL")
  val Div         = Value(0x17, "DIV")
  val Mod         = Value(0x18, "MOD")
  val Call2S      = Value(0x19, "CALL_2S")
  val Call2N      = Value(0x1a, "CALL_2N")
  val SetColour   = Value(0x1b, "SET_COLOUR")
  val Throw       = Value(0x1c, "THROW")

  def opcodeName(opnum: Int, version: Int) = {
    try {
      Oc2Op(opnum).toString
    } catch {
      case e: Exception => "(unknown 2OP opnum %02x)".format(opnum)
    }
  }
}

object Oc1Op extends Enumeration {
  val Jz         = Value(0x00, "JZ")
  val GetSibling = Value(0x01, "GET_SIBLING")
  val GetChild   = Value(0x02, "GET_CHILD")
  val GetParent  = Value(0x03, "GET_PARENT")
  val GetPropLen = Value(0x04, "GET_PROP_LEN")
  val Inc        = Value(0x05, "INC")
  val Dec        = Value(0x06, "DEC")
  val PrintAddr  = Value(0x07, "PRINT_ADDR")
  val Call1S     = Value(0x08, "CALL_1S")
  val RemoveObj  = Value(0x09, "REMOVE_OBJ")
  val PrintObj   = Value(0x0a, "PRINT_OBJ")
  val Ret        = Value(0x0b, "RET")
  val Jump       = Value(0x0c, "JUMP")
  val PrintPaddr = Value(0x0d, "PRINT_PADDR")
  val Load       = Value(0x0e, "LOAD")
  val Not        = Value(0x0f, "NOT")
  def opcodeName(opnum: Int, version: Int) = {
    if (version >= 5 && opnum == 0x0f) "CALL_1N"
    else Oc1Op(opnum).toString
  }
}

object Oc0Op extends Enumeration {
  val RTrue      = Value(0x00, "RTRUE")
  val RFalse     = Value(0x01, "RFALSE")
  val Print      = Value(0x02, "PRINT")
  val PrintRet   = Value(0x03, "PRINT_RET")
  val Nop        = Value(0x04, "NOP")
  val Save       = Value(0x05, "SAVE")
  val Restore    = Value(0x06, "RESTORE")
  val Restart    = Value(0x07, "RESTART")
  val RetPopped  = Value(0x08, "RET_POPPED")
  val Pop        = Value(0x09, "POP")
  val Quit       = Value(0x0a, "QUIT")
  val NewLine    = Value(0x0b, "NEW_LINE")
  val ShowStatus = Value(0x0c, "SHOW_STATUS")
  val Verify     = Value(0x0d, "VERIFY")
  val Piracy     = Value(0x0f, "PIRACY")
  def opcodeName(opnum: Int, version: Int) = {
    if (version >= 5 && opnum == 0x09) "CATCH"
    else Oc0Op(opnum).toString
  }
}

object OcVar extends Enumeration {
  val Call          = Value(0x00, "CALL")
  val Storew        = Value(0x01, "STOREW")
  val Storeb        = Value(0x02, "STOREB")
  val PutProp       = Value(0x03, "PUT_PROP")
  val Sread         = Value(0x04, "SREAD")
  val PrintChar     = Value(0x05, "PRINT_CHAR")
  val PrintNum      = Value(0x06, "PRINT_NUM")
  val Random        = Value(0x07, "RANDOM")
  val Push          = Value(0x08, "PUSH")
  val Pull          = Value(0x09, "PULL")
  val SplitWindow   = Value(0x0a, "SPLIT_WINDOW")
  val SetWindow     = Value(0x0b, "SET_WINDOW")
  val CallVs2       = Value(0x0c, "CALL_VS2")
  val EraseWindow   = Value(0x0d, "ERASE_WINDOW")
  val EraseLine     = Value(0x0e, "ERASE_LINE")
  val SetCursor     = Value(0x0f, "SET_CURSOR")
  val GetCursor     = Value(0x10, "GET_CURSOR")
  val SetTextStyle  = Value(0x11, "SET_TEXT_STYLE")
  val BufferMode    = Value(0x12, "BUFFER_MODE")
  val OutputStream  = Value(0x13, "OUTPUT_STREAM")
  val InputStream   = Value(0x14, "INPUT_STREAM")
  val SoundEffect   = Value(0x15, "SOUND_EFFECT")
  val ReadChar      = Value(0x16, "READ_CHAR")
  val ScanTable     = Value(0x17, "SCAN_TABLE")
  val Not           = Value(0x18, "NOT")
  val CallVn        = Value(0x19, "CALL_VN")
  val CallVn2       = Value(0x1a, "CALL_VN2")
  val Tokenise      = Value(0x1b, "TOKENISE")
  val EncodeText    = Value(0x1c, "ENCODE_TEXT")
  val CopyTable     = Value(0x1d, "COPY_TABLE")
  val PrintTable    = Value(0x1e, "PRINT_TABLE")
  val CheckArgCount = Value(0x1f, "CHECK_ARG_COUNT")
  def opcodeName(opnum: Int, version: Int) = {
    OcVar(opnum).toString
  }
}

object OcExt extends Enumeration {
  val Save         = Value(0x00, "SAVE")
  val Restore      = Value(0x01, "RESTORE")
  val LogShift     = Value(0x02, "LOG_SHIFT")
  val ArtShift     = Value(0x03, "ART_SHIFT")
  val SetFont      = Value(0x04, "SET_FONT")
  val DrawPicture  = Value(0x05, "DRAW_PICTURE")
  val PictureData  = Value(0x06, "PICTURE_DATA")
  val ErasePicture = Value(0x07, "ERASE_PICTURE")
  val SetMargins   = Value(0x08, "SET_MARGINS")
  val SaveUndo     = Value(0x09, "SAVE_UNDO")
  val RestoreUndo  = Value(0x0a, "RESTORE_UNDO")
  val PrintUnicode = Value(0x0b, "PRINT_UNICODE")
  val CheckUnicode = Value(0x0c, "CHECK_UNICODE")
  val MoveWindow   = Value(0x10, "MOVE_WINDOW")
  val WindowSize   = Value(0x11, "WINDOW_SIZE")
  val WindowStyle  = Value(0x12, "WINDOW_STYLE")
  val GetWindProp  = Value(0x13, "GET_WIND_PROP")
  val ScrollWindow = Value(0x14, "SCROLL_WINDOW")
  val PopStack     = Value(0x15, "POP_STACK")
  val ReadMouse    = Value(0x16, "READ_MOUSE")
  val MouseWindow  = Value(0x17, "MOUSE_WINDOW")
  val PushStack    = Value(0x18, "PUSH_STACK")
  val PutWindProp  = Value(0x19, "PUT_WIND_PROP")
  val PrintForm    = Value(0x1a, "PRINT_FORM")
  val MakeMenu     = Value(0x1b, "MAKE_MENU")
  val PictureTable = Value(0x1c, "PICTURE_TABLE")
  def opcodeName(opnum: Int, version: Int) = {
    OcExt(opnum).toString
  }
}
