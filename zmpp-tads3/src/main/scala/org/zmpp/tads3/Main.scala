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
  // frame pointer offsets, note that they are "off by one" compared to
  // the reference implementation because our stack organiztion is
  // slightly different
  val FpOffsetArg1           = -9
  val FpOffsetTargetProp     = -8
  val FpOffsetOriginalTarget = -7
  val FpOffsetDefiningObject = -6
  val FpOffsetSelf           = -5
}
class TadsVMState {
  import TadsVMState._
  private var _memory : Memory = null
  var image: TadsImage         = null
  val stack                    = new Stack
  val objectSystem             = new ObjectSystem(this)
  var runState                 = RunStates.Running
  var startTime : Long         = 0
  
  // Registers (TODO: current savepoint, savepoint count)
  var r0: TadsValue = null // data register R0
  var ip = 0                // instruction pointer
  var ep = 0                // current function entry pointer
  def sp = stack.sp         // stack pointer
  def sp_=(newsp: Int) {
    stack.sp = newsp
  } 
  var fp = 0                // frame pointer

  def reset(imageMem: Memory) {
    startTime = System.currentTimeMillis
    image     = new TadsImage(imageMem)
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

  def doReturn {
    sp = fp
    fp = stack.pop.value
    val argc = stack.pop.value
    ep = stack.pop.value
    val callerOffset = stack.pop.value
    // clean the rest of this invocation from the stack (self, definingObj,
    // origTargetObj, targetProp and args)
    for (i <- 0 until (argc + 4)) stack.pop
    if (callerOffset == 0) {
      throw new UnsupportedOperationException("TODO halt machine")
    }
    ip = ep + callerOffset
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
  def getParam(index: Int) = stack.valueAt(fp + FpOffsetArg1 - index)

  // local variable access. Note that Local variable access is based
  // on index 0 !!
  def getLocal(localNumber: Int) = stack.valueAt(fp + localNumber)
  def setLocal(localNumber: Int, value: TadsValue) {
    stack.setValueAt(fp + localNumber, value)
  }
  def currentSelf = stack.valueAt(fp + FpOffsetSelf)
  def currentSelf_=(value: TadsValue) = stack.setValueAt(fp + FpOffsetSelf, value)

  def targetProperty = {
    val prop = stack.valueAt(fp + FpOffsetTargetProp)
    if (prop == TadsNil) InvalidPropertyId else prop
  }
  def originalTarget = {
    val obj = stack.valueAt(fp + FpOffsetOriginalTarget)
    if (obj == TadsNil) InvalidObjectId else obj
  }
  def definingObject = {
    val obj = stack.valueAt(fp + FpOffsetDefiningObject)
    if (obj == TadsNil) InvalidObjectId else obj
  }
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
  def nextSignedByteOperand  = Types.signExtend8(nextByteOperand)

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
      case CallProp     => callProp(nextByteOperand, _state.stack.pop,
                                    nextShortOperand)
      case Dup          => _state.stack.dup
      case GetArg1      =>
        _state.stack.push(_state.getParam(nextByteOperand))
      case GetArg2      => _state.stack.push(_state.getParam(nextShortOperand))
      case GetLcl1      => _state.stack.push(_state.getLocal(nextByteOperand))
      case GetProp      => callProp(0, _state.stack.pop, nextShortOperand)
      case GetPropLcl1  => callProp(0, _state.getLocal(nextByteOperand), nextShortOperand)
      case GetPropR0    => callProp(0, _state.r0, nextShortOperand)
      case GetPropSelf  => callProp(0, _state.currentSelf, nextShortOperand)
      case GetR0        => _state.stack.push(_state.r0)
      case IdxInt8      => index(_state.stack.pop, nextByteOperand)
      case IdxLcl1Int8  => index(_state.getLocal(nextByteOperand), nextByteOperand)
      case Jf           => branchIfTrue(!_state.stack.pop.isTrue)
      case Jgt          =>
        // note the order of arguments, this is why we need to get them
        // explicitly
        val val2 = _state.stack.pop
        val val1 = _state.stack.pop
        branchIfTrue(compare(val1, val2) > 0)
      case Jmp          => _state.doBranch
      case JNil         => branchIfTrue(_state.stack.pop == TadsNil)
      case JR0T         => branchIfTrue(_state.r0.isTrue)
      case JR0F         => branchIfTrue(!_state.r0.isTrue)
      case New1         =>
        _state.r0 = _state.objectSystem.createFromStack(nextByteOperand, nextByteOperand)
      case Nop          => // do nothing
      case ObjCallProp  =>
        callProp(nextByteOperand, new TadsObjectId(nextIntOperand),
                 nextShortOperand)
      case ObjGetProp   => callProp(0, new TadsObjectId(nextIntOperand),
                                    nextShortOperand)
      case OneLcl1      => _state.setLocal(nextByteOperand, TadsInteger.One)
      case PtrCall      => ptrCall(nextByteOperand)
      case PtrInherit   => inheritProperty(nextByteOperand, _state.stack.pop)
      case Push1        => _state.stack.push1
      case PushCtxEle   => pushCtxEle(nextByteOperand)
      case PushFnPtr    => _state.stack.pushFunctionPointer(nextIntOperand)
      case PushInt8     => _state.stack.pushInt(nextSignedByteOperand)
      case PushNil      => _state.stack.pushNil
      case PushObj      => _state.stack.pushObjectId(nextIntOperand)
      case PushSelf     => _state.stack.push(_state.currentSelf)
      case PushTrue     => _state.stack.push(TadsTrue)
      case Ret          => _state.doReturn
      case RetNil       =>
        _state.r0 = TadsNil
        _state.doReturn
      case RetTrue      =>
        _state.r0 = TadsTrue
        _state.doReturn
      case RetVal       =>
        _state.r0 = _state.stack.pop
        _state.doReturn
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
      case SetProp      =>
        objSetProp(_state.stack.pop, nextShortOperand, _state.stack.pop)
      case SetPropSelf  =>
        objSetProp(_state.currentSelf, nextShortOperand,
                   _state.stack.pop)
      case SetSelf      => _state.currentSelf = _state.stack.pop
      case _            =>
        throw new UnsupportedOperationException("unknown opcode: 0x%02x"
                                                .format(opcode))
    }
    // DEBUGGING
    if (iteration >= 268) {
      println("R0 = " + _state.r0)
      println(_state.stack)
    }
  }

  // generic comparison function on TadsValues
  // used by conditional branches and comparison instructions
  // < 0 => value1 < value2
  //   0 => value1 == value2
  // > 0 => value1 > value2
  private def compare(value1: TadsValue, value2: TadsValue): Int = {
    import TypeIds._
    if (value1.valueType == VmInt && value2.valueType == VmInt) {
      value1.value - value2.value
    } else if ((value1.valueType == VmSString || value1.valueType == VmDString) &&
               (value2.valueType == VmSString || value2.valueType == VmDString)) {
      throw new UnsupportedOperationException("TODO string compare")
    } else if (value1.valueType == VmObj) {
      throw new UnsupportedOperationException("TODO object compare")
    } else throw new InvalidComparisonException
  }

  // instruction implementations
  private def index(targetValue: TadsValue, indexVal: Int) {
    if (targetValue.valueType == TypeIds.VmList) {
      throw new UnsupportedOperationException("indexing lists not supported yet")
    } else if (targetValue.valueType == TypeIds.VmObj) {
      val pushValue =
        _state.objectSystem.objectWithId(targetValue).valueAtIndex(indexVal)
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
        val prop = obj.getProperty(symb.value, argc)
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

  private def objSetProp(targetVal: TadsValue, propId: Int,
                         newVal: TadsValue) {
    if (targetVal.valueType == TypeIds.VmObj) {
      val obj = _state.objectSystem.objectWithId(targetVal)
      obj.setProperty(propId, newVal)
    } else throw new ObjectValRequiredException
  }

  // one of the central functions of the VM: evaluating properties
  // the general strategy is to look up the property first
  // (pre-evaluation can happen here) and then evaluate the property
  // that was found. The reference implementation splits this into
  // a no-eval and an eval step, which is combined here for the moment
  // to see whether we find a factorization that fits better into the
  // Scala application context
  private def callProp(argc: Int, targetVal: TadsValue, propId: Int) {
    printf("callProp(%s, %d, %d)\n", targetVal, argc, propId)

    if (targetVal.valueType == TypeIds.VmObj) {
      val obj = _state.objectSystem.objectWithId(targetVal)
      printf("callProp(%s, %d, %d), obj: %s\n", targetVal, propId, argc, obj)
      val prop = obj.getProperty(propId, argc)
      if (prop != null) {
        evalProperty(targetVal.asInstanceOf[TadsObjectId], prop, argc)
      } else {
        // TODO: check if propNotDefined is available
        throw new UnsupportedOperationException("TODO: property not found, " +
                                                "check for propNotDefined")
      }
    } else if (targetVal.valueType == TypeIds.VmList) {
      // use constant list property evaluator
      // the targetValue is an offset into the list pool, not into the static
      // object pool !!!!
      if (argc > 0) throw new UnsupportedOperationException("callProp TODO list")

      val list = _state.objectSystem.listConstantWithOffset(
        targetVal.asInstanceOf[TadsListConstant])
      val listMeta = _state.objectSystem.metaClassForName("list")
      val result = listMeta.evalClassProperty(list, propId)
      _state.r0 = result
    } else if (targetVal.valueType == TypeIds.VmSString ||
               targetVal.valueType == TypeIds.VmDString) {
      throw new UnsupportedOperationException("Cannot handle string constants yet")
    } else throw new ObjectValRequiredException
  }

  private def evalProperty(self: TadsObjectId, property: Property, argc: Int) {
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
        _state.doCall(argc, property.value, property.id, self,
                      property.definingObject, self)
      case VmDString =>
        throw new UnsupportedOperationException("TODO: DOUBLE QUOTED STRING")
      case _ =>
        throw new UnsupportedOperationException(
          "UNHANDLED TYPE: %d => STORE %d IN R0\n".format(property.valueType,
                                                          property.value))
    }
  }

  private def inheritProperty(argc: Int, propId: TadsValue) {
    val definingObject = _state.definingObject
    val originalTarget = _state.originalTarget
    val selfId         = _state.currentSelf
    val self           = _state.objectSystem.objectWithId(selfId)
    printf("inheritProperty(%d, %s), defobj = %s, orig = %s, selfId = %s self = %s\n",
           argc, propId, definingObject, originalTarget, selfId, self)
    throw new UnsupportedOperationException("inheritProperty not implemented yet")
  }

  private def pushCtxEle(elem: Int) {
    printf("PUSHCTXELE, ELEM = %d HA\n", elem)
    elem match { // not reused, so I just use the constants directly
      case 1 => _state.stack.push(_state.targetProperty)
      case 2 => _state.stack.push(_state.originalTarget)
      case 3 => _state.stack.push(_state.definingObject)
      case _ => throw new IllegalArgumentException("elem: " + elem)
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
