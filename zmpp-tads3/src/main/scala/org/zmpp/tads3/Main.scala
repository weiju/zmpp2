/*
 * Created on 2010/05/05
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
package org.zmpp.tads3

import java.util.logging._
import java.io.File
import java.io.FileInputStream

import org.zmpp.base.Types
import org.zmpp.base.Memory
import org.zmpp.base.DefaultMemory

object RunStates {
  val Running = 1
  val Halted  = 0
}
object T3VMFuncs {
  val RunGC              = 0
  val SetSay             = 1
  val GetPreinitModeFlag = 5
  val GetGlobalSymbols   = 7
}

class Tads3VMState {
  private var _memory : Memory = null
  var image: Tads3Image        = null
  val stack = new Tads3Stack
  var runState = RunStates.Running
  
  // Registers (TODO: current savepoint, savepoint count)
  var r0: Tads3Value = null // data register R0
  var ip = 0                // instruction pointer
  var ep = 0                // current function entry pointer
  def sp = stack.sp         // stack pointer
  var fp = 0                // frame pointer

  def reset(imageMem: Memory) {
    image = new Tads3Image(imageMem)

    // call initial function
    // push empty list on stack - QTads puts command line arguments in that list
    val argList = new Tads3ListConstant
    stack.push(argList)
    doCall(1, image.startEntryPoint, 0, 0, 0, 0)
  }

  def nextCodeByte = {
    val retval = image.codeByteAt(ip)
    ip += 1
    retval
  }
  def nextCodeShort = {
    val retval = image.codeShortAt(ip)
    ip += 2
    retval
  }
  def nextCodeInt = {
    val retval = image.codeIntAt(ip)
    ip += 4
    retval
  }

  def doBranch {
    val branchOffset = Types.signExtend16(image.codeShortAt(ip))
    ip += branchOffset
  }

  def doCall(argc: Int, targetOffs: Int,
             targetProp: Int, origTargetObj: Int, definingObj: Int,
             self: Int) {

    val methodHeader = image.methodHeaderAt(targetOffs)
/*
    printf("# params = %d\n", methodHeader.paramCount)
    printf("# locals = %d\n", methodHeader.localCount)
    printf("max stack slots = %d\n", methodHeader.maxStackSlots)
    printf("ex tab offs = %d\n", methodHeader.exceptionTableOffset)
    printf("debug rec offs = %d\n", methodHeader.debugRecordOffset)*/
    r0 = Tads3Nil

    // allocate stack frame
    stack.pushPropertyId(targetProp)
    if (self == 0) { // VMInvalidObj
      stack.pushNil
      stack.pushNil
      stack.pushNil
    } else {
      stack.pushObjectId(origTargetObj)
      stack.pushObjectId(definingObj)
      stack.pushObjectId(self)
    }
    // TODO: discard self object if necessary ?
    stack.pushCodeOffset(ip) // check this !!!
    stack.pushCodeOffset(ep)
    stack.pushInt(argc)
    stack.pushInt(fp)
    fp = sp
    
    // locals
    for (i <- 0 until methodHeader.localCount) stack.pushNil

    // new frame pointer
    ep = targetOffs
    ip = targetOffs + image.methodHeaderSize
  }
  
  def getParam(index: Int) = stack.valueAt(fp - 1 - index)

  // local variable access. Note that Local variable access is based
  // on index 0 !!
  def getLocal(localNumber: Int) = stack.valueAt(fp + localNumber)
  def setLocal(localNumber: Int, value: Tads3Value) {
    stack.setValueAt(fp + localNumber, value)
  }
  def currentSelf = stack.valueAt(fp - 5)
}

class Tads3VM {
  val _state = new Tads3VMState
  val _objectManager = new ObjectManager(_state)
  var sayFuncPtr: Tads3Value = null
  var iteration = 1

  def init(imageMem: Memory) {
    _state.reset(imageMem)
    _objectManager.resetImage
    printf("VALID FILE: %b, Version: %d Timestamp: %s\n",
           _state.image.isValid, _state.image.version, _state.image.timestamp)
  }
  
  def nextByteOperand   = _state.nextCodeByte
  def nextShortOperand  = _state.nextCodeShort
  def nextIntOperand    = _state.nextCodeInt
  def nextSignedShortOperand = Types.signExtend16(nextShortOperand)

  def doTurn {
    while (_state.runState == RunStates.Running) {
      executeInstruction
    }
  }
  
  def executeInstruction {
    val opcode = _state.nextCodeByte

    // debug
    printf("%04d: %s[%02x]\n", iteration, OpcodeNames.opcodeName(opcode), opcode)
    iteration += 1
    // debug

    import Opcodes._
    opcode match {
      case BP           =>
        throw new UnsupportedOperationException("Breakpoints not supported")
      case BuiltinA     => callBuiltin0(nextByteOperand, nextByteOperand)
      case Call         =>
        _state.doCall(nextByteOperand, nextIntOperand, 0, 0, 0, 0)
      case Dup          => _state.stack.dup
      case GetArg1      => _state.stack.push(_state.getParam(nextByteOperand))
      case GetLcl1      => _state.stack.push(_state.getLocal(nextByteOperand))        
      case GetPropSelf  =>
        objGetProp(_state.currentSelf.value, nextShortOperand)
      case GetR0        => _state.stack.push(_state.r0)
      case JNil         => branchIfTrue(_state.stack.pop == Tads3Nil)
      case JR0T         => branchIfTrue(_state.r0.isTrue)
      case New1         =>
        _state.r0 = _objectManager.createFromStack(nextByteOperand, nextByteOperand)
      case Nop          => // do nothing
      case ObjGetProp   => objGetProp(nextIntOperand, nextShortOperand)
      case Push1        => _state.stack.push1
      case PushFnPtr    => _state.stack.pushFunctionPointer(nextIntOperand)
      case PushNil      => _state.stack.pushNil
      case PushSelf     => _state.stack.push(_state.currentSelf)
      case SetInd       =>
        val indexVal     = _state.stack.pop
        val containerVal = _state.stack.pop
        val newVal       = _state.stack.pop
        throw new UnsupportedOperationException("SETIND not supported")
      case SetIndLcl1I8 =>
        val localNumber  = nextByteOperand
        val containerVal = _state.getLocal(localNumber)
        val index        = nextByteOperand
        val newVal       = _state.stack.pop
        _state.setLocal(localNumber, setInd(containerVal, index, newVal))
      case SetLcl1      => _state.setLocal(nextByteOperand, _state.stack.pop)
      case SetLcl1R0    => _state.setLocal(nextByteOperand, _state.r0)
      case _            =>
        throw new UnsupportedOperationException("unknown opcode: 0x%02x"
                                                .format(opcode))
    }
  }

  private def setInd(containerVal: Tads3Value, index: Int, newVal: Tads3Value) = {
    if (containerVal.valueType == TypeIds.VmObj) {
      val obj = _objectManager.objectWithId(containerVal.value)
      obj.setValueAtIndex(index, newVal)
    } else if (containerVal.valueType == TypeIds.VmList) {
      throw new UnsupportedOperationException("SETINDxxx not supported " +
                                              "for objects of list yet")
    } else throw new CannotIndexTypeException
  }

  private def branchIfTrue(condition: Boolean) {
    if (condition) _state.doBranch
    else nextShortOperand // skip branch word
  }

  // Intrinsic function set 0: t3vm
  private def callBuiltin0(argc: Int, funcIndex: Int) {
    import T3VMFuncs._
    funcIndex match {
      case SetSay =>
        if (argc != 1) throw new IllegalArgumentException("setSay() argc must be 1")
        setSay(_state.stack.pop)
      case GetGlobalSymbols =>
        _state.r0 = Tads3Nil // TODO: our test game does not have a GSYM
      case GetPreinitModeFlag => _state.r0 = Tads3Nil // never in preinit mode
      case _ =>
        throw new UnsupportedOperationException("unknown function index: %d"
                                                .format(funcIndex))
    }
  }

  private def setSay(funcPtr: Tads3Value) {
    sayFuncPtr = funcPtr
  }

  private def objGetProp(objId: Int, propId: Int) {
    val obj = _objectManager.objectWithId(objId)
    val prop = obj.findProperty(propId)
    if (prop != null) evalProperty(objId, prop)
    else {
      // TODO: check if propNotDefined is available
      throw new UnsupportedOperationException("TODO: property not found, " +
                                              "check for propNotDefined")
    }
  }

  private def evalProperty(selfId: Int, property: Property) {
    import TypeIds._
    printf("evalProperty(%s) [self = %d]\n", property, selfId)
    property.valueType match {
      case VmNil     => _state.r0 = Tads3Nil
      case VmTrue    => _state.r0 = Tads3True
      case VmObj     => _state.r0 = new Tads3ObjectId(property.value)
      case VmProp    => _state.r0 = new Tads3PropertyId(property.value)
      case VmInt     => _state.r0 = new Tads3Integer(property.value)
      case VmCodeOfs =>
        _state.doCall(0, property.value, 0, 0, property.definingObject, selfId)
      case VmDString =>
        throw new UnsupportedOperationException("TODO: DOUBLE QUOTED STRING")
      case _ =>
        throw new UnsupportedOperationException(
          "UNHANDLED TYPE: %d => STORE %d IN R0\n".format(property.valueType,
                                                          property.value))
    }
  }
}

object Tads3Main {
  private var _vm : Tads3VM = null

  def readFileData(file: File) = {
    val filebytes = new Array[Byte](file.length.toInt)
    var fileIs : FileInputStream = null
    try {
      fileIs = new FileInputStream(file)
      fileIs.read(filebytes)
    } finally {
      if (fileIs != null) fileIs.close
    }
    filebytes
  }
  def readTads3File(file : File) = {
    // Little Endian format - Hello Intel-Lovers
    val imageMem = new DefaultMemory(readFileData(file)).littleEndian
    _vm = new Tads3VM
    _vm.init(imageMem)
    _vm
  }
  
  def main(args: Array[String]) {
    println("ZMPP TADS3 (Prototype version)")
    val vm = readTads3File(new File(args(0)))
    println("\nProgram:\n---------")
    vm.doTurn
  }
}
