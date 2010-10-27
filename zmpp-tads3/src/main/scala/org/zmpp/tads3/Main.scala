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

object TadsVMState {
  val StackOffsetArg1 = -9
}
class TadsVMState {
  private var _memory : Memory = null
  var image: TadsImage        = null
  val stack = new Stack
  val objectSystem = new ObjectSystem(this)
  var runState = RunStates.Running
  
  // Registers (TODO: current savepoint, savepoint count)
  var r0: TadsValue = null // data register R0
  var ip = 0                // instruction pointer
  var ep = 0                // current function entry pointer
  def sp = stack.sp         // stack pointer
  var fp = 0                // frame pointer

  def reset(imageMem: Memory) {
    image = new TadsImage(imageMem)
    objectSystem.reset
    image.readData(this)

    // call initial function
    // push empty list on stack - QTads puts command line arguments in that list
    val argList = new TadsListConstant(0)
    stack.push(argList)
    doCall(1, image.startEntryPoint, 0,
           InvalidObjectId, InvalidObjectId, InvalidObjectId)
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
             targetProp: Int, origTargetObj: TadsObjectId,
             definingObj: TadsObjectId, self: TadsObjectId) {

    val methodHeader = image.methodHeaderAt(targetOffs)
    r0 = TadsNil

    // allocate stack frame
    // The TM says push nil if there is no target property, the reference
    // implementation pushes property id 0, we do that, too, for the moment
    stack.pushPropertyId(targetProp)
    if (self == InvalidObjectId) { // VMInvalidObj
      stack.pushNil // target object
      stack.pushNil // defining object
      stack.pushNil // self object
    } else {
      stack.push(origTargetObj)
      stack.push(definingObj)
      stack.push(self)
    }
    // The spec says 'compute the offset from the current method header of
    // the next instruction execute and push the result'. This simply means
    // push CodeOfs(Instruction Pointer - Entry Pointer)
    stack.pushCodeOffset(ip - ep)
    stack.pushCodeOffset(ep)
    stack.pushInt(argc)
    stack.pushStackRef(fp)
    fp = sp
    
    // locals
    for (i <- 0 until methodHeader.localCount) stack.pushNil

    // new frame pointer
    ep = targetOffs
    ip = targetOffs + image.methodHeaderSize
  }
  
  // function argument acccess, indexing is 0-based
  def getParam(index: Int) = stack.valueAt(fp + TadsVMState.StackOffsetArg1 - index)

  // local variable access. Note that Local variable access is based
  // on index 0 !!
  def getLocal(localNumber: Int) = stack.valueAt(fp + localNumber)
  def setLocal(localNumber: Int, value: TadsValue) {
    stack.setValueAt(fp + localNumber, value)
  }
  def currentSelf = stack.valueAt(fp - 5)
  def currentSelf_=(value: TadsValue) = stack.setValueAt(fp - 5, value)
}

class TadsVM {
  val _state                 = new TadsVMState
  val _functionSetMapper     = new IntrinsicFunctionSetMapper
  var iteration              = 1

  def init(imageMem: Memory) {
    _state.reset(imageMem)
    _functionSetMapper.reset(_state)
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
      case BuiltinA     =>
        _functionSetMapper.callBuiltin(nextByteOperand, nextByteOperand, 0)
      case BuiltinB     =>
        _functionSetMapper.callBuiltin(nextByteOperand, nextByteOperand, 1)
      case BuiltinC     =>
        _functionSetMapper.callBuiltin(nextByteOperand, nextByteOperand, 2)
      case BuiltinD     =>
        _functionSetMapper.callBuiltin(nextByteOperand, nextByteOperand, 3)
      case Builtin1     =>
        _functionSetMapper.callBuiltin(nextByteOperand, nextByteOperand,
                                       nextByteOperand)
      case Builtin2     =>
        _functionSetMapper.callBuiltin(nextByteOperand, nextShortOperand,
                                       nextByteOperand)
      case Call         =>
        _state.doCall(nextByteOperand, nextIntOperand, 0,
                      InvalidObjectId, InvalidObjectId, InvalidObjectId)
      case Dup          => _state.stack.dup
      case GetArg1      =>
        _state.stack.push(_state.getParam(nextByteOperand))
      case GetArg2      => _state.stack.push(_state.getParam(nextShortOperand))
      case GetLcl1      => _state.stack.push(_state.getLocal(nextByteOperand))
      case GetProp      => objGetProp(_state.stack.pop, nextShortOperand)
      case GetPropR0    => objGetProp(_state.r0, nextShortOperand)
      case GetPropSelf  =>
        objGetProp(_state.currentSelf, nextShortOperand)
      case GetR0        => _state.stack.push(_state.r0)
      case IdxInt8      => index(nextByteOperand)
      case JNil         => branchIfTrue(_state.stack.pop == TadsNil)
      case JR0T         => branchIfTrue(_state.r0.isTrue)
      case New1         =>
        _state.r0 = _state.objectSystem.createFromStack(nextByteOperand, nextByteOperand)
      case Nop          => // do nothing
      case ObjGetProp   => objGetProp(new TadsObjectId(nextIntOperand),
                                      nextShortOperand)
      case PtrCall      => ptrCall(nextByteOperand)
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
      case SetSelf      => _state.currentSelf = _state.stack.pop
      case _            =>
        throw new UnsupportedOperationException("unknown opcode: 0x%02x"
                                                .format(opcode))
    }
    // DEBUGGING
    //println("R0 = " + _state.r0)
    //println(_state.stack)
  }

  // instruction implementations
  private def index(indexVal: Int) {
    val value = _state.stack.pop
    if (value.valueType == TypeIds.VmList) {
      throw new UnsupportedOperationException("indexing lists not supported yet")
    } else if (value.valueType == TypeIds.VmObj) {
      val pushValue =
        _state.objectSystem.objectWithId(value).valueAtIndex(indexVal)
      _state.stack.push(pushValue)
    } else throw new CannotIndexTypeException
  }

  private def ptrCall(argc: Int) {
    val stackVal = _state.stack.pop
    if (stackVal.valueType == TypeIds.VmProp) {
      throw new UnsupportedOperationException("PtrCall with PROP not supported yet")
    } else if (stackVal.valueType == TypeIds.VmObj) {
      printf("OBJ PROP CALL, OBJ = %d\n", stackVal.value)
      val obj  = _state.objectSystem.objectWithId(stackVal.value)
      val symb = _state.image.symbolicNames("ObjectCallProp")
      if (symb != null && symb.valueType == TypeIds.VmProp) {
        printf("SYM: %s TYP: %d VAL: %d\n", symb.name, symb.valueType, symb.value)
        val prop = obj.findProperty(symb.value)
        _state.doCall(argc, prop.value, 0, obj.id, obj.id, obj.id)
      } else throw new FuncPtrValRequiredException
    } else if (stackVal.valueType == TypeIds.VmFuncPtr) {
      throw new UnsupportedOperationException("PtrCall with FuncPtr not supported yet")
    } else throw new FuncPtrValRequiredException
  }

  private def setInd(containerVal: TadsValue, index: Int, newVal: TadsValue) = {
    if (containerVal.valueType == TypeIds.VmObj) {
      val obj = _state.objectSystem.objectWithId(containerVal.value)
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

  private def objGetProp(targetVal: TadsValue, propId: Int) {
    if (targetVal.valueType == TypeIds.VmObj) {
      val obj = _state.objectSystem.objectWithId(targetVal)
      printf("objGetProp(%s, %d), obj: %s\n", targetVal, propId, obj)
      val prop = obj.findProperty(propId)
      if (prop != null) evalProperty(targetVal.asInstanceOf[TadsObjectId], prop)
      else {
        // TODO: check if propNotDefined is available
        throw new UnsupportedOperationException("TODO: property not found, " +
                                                "check for propNotDefined")
      }
    } else if (targetVal.valueType == TypeIds.VmList) {
      // use constant list property evaluator
      // the targetValue is an offset into the list pool, not into the static
      // object pool !!!!
      // val obj = TODO
      val list = null // TODO
      val listMeta = _state.objectSystem.metaClassForName("list")
      listMeta.evalClassProperty(list, propId)
      throw new UnsupportedOperationException("cannot handle list constants yet")
    } else if (targetVal.valueType == TypeIds.VmSString ||
               targetVal.valueType == TypeIds.VmDString) {
      throw new UnsupportedOperationException("Cannot handle string constants yet")
    } else throw new ObjectValRequiredException
  }

  private def evalProperty(self: TadsObjectId, property: Property) {
    import TypeIds._
    printf("evalProperty(%s) [self = %s]\n", property, self)
    property.valueType match {
      case VmNil     => _state.r0 = TadsNil
      case VmTrue    => _state.r0 = TadsTrue
      case VmObj     => _state.r0 = new TadsObjectId(property.value)
      case VmProp    => _state.r0 = new TadsPropertyId(property.value)
      case VmInt     => _state.r0 = new TadsInteger(property.value)
      case VmList    => _state.r0 = new TadsListConstant(property.value)
      case VmCodeOfs =>
        _state.doCall(0, property.value, property.id, self,
                      property.definingObject, self)
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
  private var _vm : TadsVM = null

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
    _vm = new TadsVM
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
