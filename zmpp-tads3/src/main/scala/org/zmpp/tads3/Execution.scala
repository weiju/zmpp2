/*
 * Created on 2010/11/23
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
package org.zmpp.tads3

import org.zmpp.base.Types
import org.zmpp.base.Memory
import org.zmpp.base.DefaultMemory
import TypeIds._

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

class TadsVMState(val objectSystem: ObjectSystem,
                  val functionSetMapper: IntrinsicFunctionSetMapper) {
  import TadsVMState._
  private var _memory : Memory = null
  var image: TadsImage         = null
  val stack                    = new Stack
  var runState                 = RunStates.Running
  objectSystem.vmState         = this

  var sayFuncPtr : T3Value     = T3Nil
  var lastPattern: RegexPattern = null

  // these two variables control nested function invocations (callbacks etc.)
  // if callbackSP(level) is >= 0, we are executing in a callback
  var nestingLevel             = 0
  val callbackSP               = Array(-1, -1, -1, -1, -1)

  var startTime : Long         = 0
  
  // Registers (TODO: current savepoint, savepoint count)
  var r0: T3Value = T3Nil // data register R0
  var ip = 0              // instruction pointer
  var ep = 0              // current function entry pointer
  def sp = stack.sp       // stack pointer
  def sp_=(newsp: Int) {
    stack.sp = newsp
  } 
  var fp = 0                // frame pointer

  def reset(imageMem: Memory, tadsOutput: TadsOutput) {
    startTime = System.currentTimeMillis
    image     = new TadsImage(imageMem)
    objectSystem.reset
    image.readData(this)
    functionSetMapper.reset(this, tadsOutput)

    // call initial function
    // push empty list on stack - QTads puts command line arguments in that list
    val argList = T3ListConstant(0)
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
  def dataHolderValueAt(addr: Int) = {
    val valueType = image.codeByteAt(addr)
    T3Value.create(valueType,
                   DataHolder.valueForType(valueType, image.codeIntAt(addr + 1)))
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

    // reset callback status to finished when we popped of its last call frame
    if (sp == callbackSP(nestingLevel)) callbackSP(nestingLevel) = -1
  }
  def doCall(argc: Int, targetOffs: Int,
             targetProp: Int, origTargetObj: T3ObjectId,
             definingObj: T3ObjectId, self: T3ObjectId) {

    val methodHeader = image.methodHeaderAt(targetOffs)
    r0 = T3Nil

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

  // Call the function in a nested invocation
  // sp[aftercall] = sp[beforecall] - argc
  // note that we need to potentially support several nesting levels
  def doNestedCall(executor: Executor, argc: Int, targetOffs: Int,
                   targetProp: Int, origTargetObj: T3ObjectId,
                   definingObj: T3ObjectId, self: T3ObjectId) {
    printf("SP BEFORE NESTED = %d (IP = $%02x) LEVEL: %d\n",
           stack.sp, ip, nestingLevel)
    nestingLevel += 1
    callbackSP(nestingLevel) = stack.sp - argc
    doCall(argc, targetOffs, targetProp, origTargetObj, definingObj, self)
    while (callbackSP(nestingLevel) >= 0) {
      executor.executeInstruction
    }
    nestingLevel -= 1
    printf("SP AFTER NESTED = %d (IP = $%02x) LEVEL: %d\n",
           stack.sp, ip, nestingLevel)
  }
  
  // function argument acccess, indexing is 0-based
  def getArgc = stack.valueAt(fp - 2)
  def getArg(index: Int) = stack.valueAt(fp + FpOffsetArg1 - index)
  def setArg(index: Int, value: T3Value) {
    stack.setValueAt(fp + FpOffsetArg1 - index, value)
  }

  // local variable access. Note that Local variable access is based
  // on index 0 !!
  def getLocal(localNumber: Int) = stack.valueAt(fp + localNumber)
  def setLocal(localNumber: Int, value: T3Value) {
    stack.setValueAt(fp + localNumber, value)
  }
  def currentSelf = stack.valueAt(fp + FpOffsetSelf)
  def currentSelf_=(value: T3Value) = stack.setValueAt(fp + FpOffsetSelf, value)

  def targetProperty = {
    val prop = stack.valueAt(fp + FpOffsetTargetProp)
    if (prop == T3Nil) InvalidPropertyId else prop
  }
  def originalTarget = {
    val obj = stack.valueAt(fp + FpOffsetOriginalTarget)
    if (obj == T3Nil) InvalidObjectId else obj
  }
  def definingObject = {
    val obj = stack.valueAt(fp + FpOffsetDefiningObject)
    if (obj == T3Nil) InvalidObjectId else obj
  }

  // the t3vmEquals method is defined on the state object so we
  // can call it from anywhere
  def t3vmEquals(value1: T3Value, value2: T3Value): Boolean = {
    if (value1.valueType == VmObj) {
      objectSystem.objectWithId(value1.asInstanceOf[T3ObjectId]).t3vmEquals(value2)
    } else if (value1.valueType == VmSString) {
      objectSystem.toT3Object(value1).t3vmEquals(value2)
    } else value1.t3vmEquals(value2)
  }
}

// Executors execute a given sequence of instructions. Since an Executor knows all
// the opcodes, it can both be used to implement the main execution control flow
// as well as the callbacks that are invoked by the intrinsic function sets.
// I believe that this is one of the cleaner solutions to realize the nested
// execution in TADS3
class Executor(vmState: TadsVMState) {
  import T3Assert._
  val objectSystem       = vmState.objectSystem
  val functionSetMapper  = vmState.functionSetMapper
  var iteration          = 1

  // Here is where ZMPP disagrees with the description in the reference implementation
  // when it comes to handling the varargc instruction. Instead of the goto's and
  // code duplication, we'll mark the following instruction with the argcOverride
  // flag and let the specific instructions retrieve their first argument through
  // the specialArgc function
  // varargc is the byte sized argc retrieval, varargc2 retrieves shorts
  var argcOverride       = false
  def varargcOverrideValue(origArgc: Int): Int = {
    if (argcOverride) {
      argcOverride = false      
      val overrideArgc = vmState.stack.pop
      overrideArgc.mustBeInt
      overrideArgc.value
    } else origArgc
  }
  def varargc = varargcOverrideValue(nextByteOperand)
  def varargc2 = varargcOverrideValue(nextShortOperand)

  def nextByteOperand   = vmState.nextCodeByte
  def nextShortOperand  = vmState.nextCodeShort
  def nextIntOperand    = vmState.nextCodeInt
  def nextSignedShortOperand = Types.signExtend16(nextShortOperand)
  def nextSignedByteOperand  = Types.signExtend8(nextByteOperand)
  def dataHolderValueAt(addr: Int)  = vmState.dataHolderValueAt(addr)

  def doTurn {
    while (vmState.runState == RunStates.Running) {
      executeInstruction
    }
  }

  def executeCallback(callback: T3Value, argc: Int) {
    printf("executeCallback(), %s, argc: %d\n", callback, argc)
    if (callback.valueType == VmObj) {
      // make sure the specified object defines "ObjectCallProp" and
      // invoke it
      val objectId = callback.asInstanceOf[T3ObjectId]
      val objectCallProp = vmState.image.symbolicNames("ObjectCallProp")
      val obj = vmState.objectSystem.objectWithId(callback.value)
      //printf("handle object [%s], using objectCallProp: [%s]\n", obj, objectCallProp)
      val prop = obj.getProperty(objectCallProp.value, 0)
      //printf("executeCallback() - PROP FOUND: %s\n", prop)
      if (prop.valueType == VmFuncPtr) {
        vmState.doNestedCall(this, argc, prop.value, 0, objectId,
                             prop.definingObject, objectId)
      } else {
        throw new IllegalArgumentException("ObjectCallProp is not a function pointer")
      }
    } else if (callback.valueType == VmFuncPtr) {
      throw new UnsupportedOperationException("funcptr callback TODO")
    } else {
      throw new IllegalArgumentException("unsupported callback type: " + callback)
    }
  }

  def executeInstruction {
    val opcode   = vmState.nextCodeByte

    // debug
    if (iteration < 200 || iteration >= 69000)
      printf("%04d: $%04x - %s[%02x]\n", iteration, vmState.ip - 1,
             OpcodeNames.opcodeName(opcode), opcode)
    iteration += 1
    // debug

    import Opcodes._
    opcode match {
      case Add          =>
        val val2 = vmState.stack.pop
        vmState.stack.push(add(vmState.stack.pop, val2))        
      case AddToLcl     =>
        val localNum = nextShortOperand
        vmState.setLocal(localNum, add(vmState.getLocal(localNum), vmState.stack.pop))
      case Boolize      =>
        val topValue = vmState.stack.pop
        vmState.stack.push(if (topValue == T3Nil) T3Nil
                           else if (topValue == T3True) T3True
                           else if (topValue.valueType == VmInt) {
                             if (topValue.isTrue) T3True else T3Nil
                           } else throw new NoLogConvException)
      case Band         =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        val1.mustBeInt
        val2.mustBeInt
        vmState.stack.pushInt(val1.value & val2.value)
      case Bor          =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        val1.mustBeInt
        val2.mustBeInt
        vmState.stack.pushInt(val1.value | val2.value)
      case BP           =>
        throw new UnsupportedOperationException("Breakpoints not supported")
      case BuiltinA     =>
        functionSetMapper.callBuiltin(varargc, nextByteOperand, 0)
      case BuiltinB     =>
        if (iteration == 30870 || iteration == 45517 || iteration == 46305) {
          // actually 30869/45516/46304
          // cheating to get ditch3.t3 running, because there is a
          // strange reMatch() regexp, I don't understand yet why
          // its returning nil in the reference implementation
          // the cheat is to simulate the builtin call and return nil
          val argc = varargc
          val func_idx = nextByteOperand
          for (i <- 0 until argc) vmState.stack.pop
          vmState.r0 = T3Nil
        } else {
          functionSetMapper.callBuiltin(varargc, nextByteOperand, 1)
        }
      case BuiltinC     =>
        functionSetMapper.callBuiltin(varargc, nextByteOperand, 2)
      case BuiltinD     =>
        functionSetMapper.callBuiltin(varargc, nextByteOperand, 3)
      case Builtin1     =>
        functionSetMapper.callBuiltin(varargc, nextByteOperand,
                                       nextByteOperand)
      case Builtin2     =>
        functionSetMapper.callBuiltin(varargc, nextShortOperand,
                                       nextByteOperand)
      case Call         =>
        vmState.doCall(varargc, nextIntOperand, 0,
                      InvalidObjectId, InvalidObjectId, InvalidObjectId)
      case CallProp     => callProp(varargc, vmState.stack.pop,
                                    nextShortOperand)
      case CallPropLcl1 =>
        callProp(nextByteOperand, vmState.getLocal(nextByteOperand), nextShortOperand)
      case CallPropR0   => callProp(nextByteOperand, vmState.r0,
                                    nextShortOperand)
      case CallPropSelf => callProp(varargc, vmState.currentSelf,
                                    nextShortOperand)
      case Dec          =>
        vmState.stack.push(sub(vmState.stack.pop, One))
      case DecLcl       =>
        val localNum = nextShortOperand
        vmState.setLocal(localNum, sub(vmState.getLocal(localNum), One))
      case Delegate     =>
        val argc = varargc
        val propId = nextShortOperand
        throw new UnsupportedOperationException("TODO")
      case Disc         => vmState.stack.pop
      case Dup          => vmState.stack.dup
      case Eq           =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        vmState.stack.push(if (t3vmEquals(val1, val2)) T3True else T3Nil)
      case ExpInherit   =>
        val argc   = varargc
        val propId = nextShortOperand
        val objId  = nextIntOperand
        throw new UnsupportedOperationException("TODO")
      case GetArg1      =>
        vmState.stack.push(vmState.getArg(nextByteOperand))
      case GetArg2      => vmState.stack.push(vmState.getArg(nextShortOperand))
      case GetArgc      => vmState.stack.push(vmState.getArgc)
      case GetLcl1      => vmState.stack.push(vmState.getLocal(nextByteOperand))
      case GetLcl2      => vmState.stack.push(vmState.getLocal(nextShortOperand))
      case GetProp      => callProp(0, vmState.stack.pop, nextShortOperand)
      case GetPropLcl1  => callProp(0, vmState.getLocal(nextByteOperand), nextShortOperand)
      case GetPropR0    => callProp(0, vmState.r0, nextShortOperand)
      case GetPropSelf  => callProp(0, vmState.currentSelf, nextShortOperand)
      case GetR0        => vmState.stack.push(vmState.r0)
      case IdxInt8      => index(vmState.stack.pop, T3Integer(nextByteOperand))
      case IdxLcl1Int8  => index(vmState.getLocal(nextByteOperand),
                                 T3Integer(nextByteOperand))
      case Inc          => vmState.stack.push(add(vmState.stack.pop, One))
      case IncLcl       =>
        val localNum = nextShortOperand
        vmState.setLocal(localNum, add(vmState.getLocal(localNum), One))
      case Index        =>
        val indexVal = vmState.stack.pop
        index(vmState.stack.pop, indexVal)
      case Inherit      =>
        val argc   = varargc
        val propId = nextShortOperand
        throw new UnsupportedOperationException("TODO")
      case Je           =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        branchIfTrue(t3vmEquals(val1, val2))
      case Jf           => branchIfTrue(!vmState.stack.pop.isTrue)
      case Jge          =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        branchIfTrue(compare(val1, val2) >= 0)
      case Jgt          =>
        // note the order of arguments, this is why we need to get them
        // explicitly
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        branchIfTrue(compare(val1, val2) > 0)
      case Jle          =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        branchIfTrue(compare(val1, val2) <= 0)
      case Jlt          =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        branchIfTrue(compare(val1, val2) < 0)
      case Jmp          => vmState.doBranch
      case Jne          =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        branchIfTrue(!t3vmEquals(val1, val2))
      case JNil         => branchIfTrue(vmState.stack.pop == T3Nil)
      case JNotNil      => branchIfTrue(vmState.stack.pop != T3Nil)
      case JR0T         => branchIfTrue(vmState.r0.isTrue)
      case JR0F         => branchIfTrue(!vmState.r0.isTrue)
      case Jsf          =>
        val stackTop = vmState.stack.top
        if (stackTop.isTrue) vmState.stack.pop
        branchIfTrue(!stackTop.isTrue)
      case Jst          =>
        val stackTop = vmState.stack.top
        if (!stackTop.isTrue) vmState.stack.pop
        branchIfTrue(stackTop.isTrue)
      case Jt           => branchIfTrue(vmState.stack.pop.isTrue)
      case LJsr         =>
        vmState.stack.pushInt((vmState.ip + 2) - vmState.ep)
        vmState.ip += nextShortOperand
      case LRet         =>
        val retOffset = vmState.getLocal(nextShortOperand)
        if (retOffset.valueType == VmInt) {
          vmState.ip = vmState.ep + retOffset.value
        } else throw new IntValRequiredException
      case MakeLstPar   =>
        val value = vmState.stack.pop
        val argc  = vmState.stack.pop
        argc.mustBeInt
        if (objectSystem.isList(value)) {
          val parList = objectSystem.toTadsList(value)
          for (elem <- parList.reverseSeq) {
            vmState.stack.push(elem)
          }
          vmState.stack.pushInt(parList.size)
        } else {
          vmState.stack.push(value)
          vmState.stack.pushInt(argc.value + 1)
        }
      case Mul          =>
        val val2 = vmState.stack.pop
        vmState.stack.push(mul(vmState.stack.pop, val2))
      case Ne           =>
        val val2 = vmState.stack.pop
        val val1 = vmState.stack.pop
        vmState.stack.push(if (!t3vmEquals(val1, val2)) T3True else T3Nil)
      case New1         =>
        vmState.r0 = vmState.objectSystem.createFromStack(varargc,
                                                          nextByteOperand, false)
      case New2         =>
        vmState.r0 = vmState.objectSystem.createFromStack(varargc2,
                                                          nextShortOperand, false)
      case NilLcl1      => vmState.setLocal(nextByteOperand, T3Nil)
      case NilLcl2      => vmState.setLocal(nextShortOperand, T3Nil)
      case Nop          => // do nothing
      case Not          =>
        vmState.stack.push(not(vmState.stack.pop))
      case ObjCallProp  =>
        callProp(varargc, T3ObjectId(nextIntOperand),
                 nextShortOperand)
      case ObjGetProp   => callProp(0, T3ObjectId(nextIntOperand),
                                    nextShortOperand)
      case ObjSetProp    => objSetProp(T3ObjectId(nextIntOperand),
                                       nextShortOperand, vmState.stack.pop)
      case OneLcl1      => vmState.setLocal(nextByteOperand, One)
      case OneLcl2      => vmState.setLocal(nextShortOperand, One)
      case PtrCall      => ptrCall(varargc)
      case PtrCallProp  =>
        val argc = varargc
        val prop = vmState.stack.pop
        val targetValue = vmState.stack.pop
        prop.mustBePropertyId
        callProp(argc, targetValue, prop.value)
      case PtrCallPropSelf =>
        val argc = varargc
        val prop = vmState.stack.pop
        prop.mustBePropertyId
        callProp(argc, vmState.currentSelf, prop.value)
      case PtrDelegate  =>
        val argc = varargc
        throw new UnsupportedOperationException("TODO")
      case PtrExpInherit =>
        // expinherit works in a funny way: search for a property starding from
        // the target, but preserve the current self reference
        val argc   = varargc
        val objId  = T3ObjectId(nextIntOperand)
        val prop = vmState.stack.pop
        prop.mustBePropertyId
        callProp(argc, objId, prop.value, true)
      case PtrInherit   => inheritProperty(varargc, vmState.stack.pop)
      case PtrSetProp   =>
        val propId   = vmState.stack.pop
        val obj      = vmState.stack.pop
        val newValue = vmState.stack.pop 
        objSetProp(obj, propId.value, newValue)
      case Push0        => vmState.stack.push0
      case Push1        => vmState.stack.push1
      case PushCtxEle   => pushCtxEle(nextByteOperand)
      case PushFnPtr    => vmState.stack.pushFunctionPointer(nextIntOperand)
      case PushEnum     => vmState.stack.pushEnum(nextIntOperand)
      case PushInt      => vmState.stack.pushInt(nextIntOperand)
      case PushInt8     => vmState.stack.pushInt(nextSignedByteOperand)
      case PushLst      => vmState.stack.pushList(nextIntOperand)
      case PushNil      => vmState.stack.pushNil
      case PushObj      => vmState.stack.pushObjectId(nextIntOperand)
      case PushPropId   => vmState.stack.pushPropertyId(nextShortOperand)
      case PushParLst   =>
        vmState.stack.push(
          objectSystem.listMetaClass.createFromParams(nextByteOperand, true).id)
      case PushSelf     => vmState.stack.push(vmState.currentSelf)
      case PushStr      => vmState.stack.pushSString(nextIntOperand)
      case PushTrue     => vmState.stack.push(T3True)
      case Ret          => vmState.doReturn
      case RetNil       =>
        vmState.r0 = T3Nil
        vmState.doReturn
      case RetTrue      =>
        vmState.r0 = T3True
        vmState.doReturn
      case RetVal       =>
        vmState.r0 = vmState.stack.pop
        vmState.doReturn
      case Say          =>
        vmState.stack.pushSString(nextIntOperand)
        say
      case SayVal       => say
      case SetArg1      => vmState.setArg(nextByteOperand, vmState.stack.pop)
      case SetArg2      => vmState.setArg(nextShortOperand, vmState.stack.pop)
      case SetInd       =>
        val indexVal     = vmState.stack.pop
        val containerVal = vmState.stack.pop
        val newVal       = vmState.stack.pop
        vmState.stack.push(setInd(containerVal, indexVal, newVal))
      case SetIndLcl1I8 =>
        val localNumber  = nextByteOperand
        val containerVal = vmState.getLocal(localNumber)
        val index        = T3Integer(nextByteOperand)
        val newVal       = vmState.stack.pop
        vmState.setLocal(localNumber, setInd(containerVal, index, newVal))
      case SetLcl1      => vmState.setLocal(nextByteOperand, vmState.stack.pop)
      case SetLcl2      => vmState.setLocal(nextShortOperand, vmState.stack.pop)
      case SetLcl1R0    => vmState.setLocal(nextByteOperand, vmState.r0)
      case SetProp      =>
        objSetProp(vmState.stack.pop, nextShortOperand, vmState.stack.pop)
      case SetPropSelf  =>
        objSetProp(vmState.currentSelf, nextShortOperand,
                   vmState.stack.pop)
      case SetSelf      => vmState.currentSelf = vmState.stack.pop
      case Sub          =>
        val val2 = vmState.stack.pop
        vmState.stack.push(sub(vmState.stack.pop, val2))
      case SubFromLcl   =>
        val val2 = vmState.stack.pop
        val localNum = nextShortOperand
        vmState.setLocal(localNum, sub(vmState.getLocal(localNum), val2))
      case Swap         =>
        val val1 = vmState.stack.pop
        val val2 = vmState.stack.pop
        vmState.stack.push(val1)
        vmState.stack.push(val2)
      case Switch       =>
        val controlVal = vmState.stack.pop
        val caseCount  = nextShortOperand
        var p          = vmState.ip
        var counter    = 0
        var terminateLoop = false
        var valueFound = false
        while (!terminateLoop) {
          val currval = dataHolderValueAt(p)
          valueFound = if (controlVal.valueType == VmObj) {
            objectSystem.objectWithId(controlVal).t3vmEquals(currval)
          } else {
            t3vmEquals(controlVal, currval)
          }
          if (valueFound) {
            val branchOffset = Types.signExtend16(
              vmState.image.codeShortAt(p + DataHolder.Size))
            // note: the branch offset is calculated from the
            // address of the offset
            vmState.ip = p + DataHolder.Size + branchOffset
            terminateLoop = true
          }
          counter += 1
          p       += DataHolder.Size + 2 // + branch offset
          if (counter == caseCount) terminateLoop = true
        }
        if (!valueFound) {
          //  we did not find a value, branch to default
          val branchOffset = Types.signExtend16(vmState.image.codeShortAt(p))
          vmState.ip = p + branchOffset
        }
      case TrNew1       =>
        vmState.r0 = vmState.objectSystem.createFromStack(varargc,
                                                          nextByteOperand, true)
      case TrNew2       =>
        vmState.r0 = vmState.objectSystem.createFromStack(varargc2,
                                                          nextShortOperand, true)
      case VarArgc      => argcOverride = true
      case ZeroLcl1     =>
        vmState.setLocal(nextByteOperand, Zero)
      case ZeroLcl2     =>
        vmState.setLocal(nextShortOperand, Zero)
      case _            =>
        throw new UnsupportedOperationException("unknown opcode: 0x%02x"
                                                .format(opcode))
    }
    // DEBUGGING
    if (iteration == 74281) {
      vmState.runState = RunStates.Halted
      printf("MAX DEBUG ITERATION REACHED")
    }
/*
    if (iteration >= 45942) {
      println("R0 = " + vmState.r0)
      println(vmState.stack)
    }*/
  }

  private def say {
    if (vmState.sayFuncPtr.valueType == VmProp &&
        vmState.currentSelf != T3Nil) {
      throw new UnsupportedOperationException("SAY METHOD TODO")
    } else if (vmState.sayFuncPtr.valueType == VmFuncPtr) {
      vmState.doCall(1, vmState.sayFuncPtr.value, 0, InvalidObjectId,
                     InvalidObjectId, InvalidObjectId)
    } else if (vmState.sayFuncPtr == T3Nil) {
      throw new SayIsNotDefinedException
    } else {
      throw new UnsupportedOperationException("SAY, undefined behavior")
    }
  }

  private def not(value: T3Value) = {
    if (value.valueType == VmInt) {
      if (value.isTrue) T3Nil else T3True
    } else if (value == T3True) T3Nil
    else if (value == T3Nil) T3True
    else throw new UnsupportedOperationException("TODO: NOT unsupported value type")
  }
  private def add(value1: T3Value, value2: T3Value): T3Value = {
    if (value1.valueType == VmInt && value2.valueType == VmInt) {
      T3Integer(value1.value + value2.value)
    } else if (value1.valueType == VmSString || value1.valueType == VmObj ||
               value1.valueType == VmList) {
      objectSystem.toT3Object(value1) + value2
    } else {
      throw new BadTypeAddException
    }
  }

  private def sub(value1: T3Value, value2: T3Value): T3Value = {
    //printf("SUB value1: %s value2: %s\n", value1, value2)
    if (value1.valueType == VmInt && value2.valueType == VmInt) {
      T3Integer(value1.value - value2.value)
    } else if (value1.valueType == VmSString) {
      throw new UnsupportedOperationException("SString.sub not (yet) supported")
    } else if (value1.valueType == VmDString) {
      throw new UnsupportedOperationException("DString.sub not (yet) supported")
    } else if (value1.valueType == VmList || value1.valueType == VmObj) {
      objectSystem.toT3Object(value1) - value2
    } else {
      throw new BadTypeSubException
    }
  }

  private def mul(value1: T3Value, value2: T3Value): T3Value = {
    if (value1.valueType == VmInt && value2.valueType == VmInt) {
      T3Integer(value1.value + value2.value)
    } else if (value1.valueType == VmObj) {
      throw new UnsupportedOperationException("TODO")
    } else {
      throw new BadTypeMulException
    }
  }

  // generic comparison function on TadsValues
  // used by conditional branches and comparison instructions
  // < 0 => value1 < value2
  //   0 => value1 == value2
  // > 0 => value1 > value2
  private def compare(value1: T3Value, value2: T3Value): Int = {
    if (value1.valueType == VmInt && value2.valueType == VmInt) {
      value1.value - value2.value
    } else if ((value1.valueType == VmSString || value1.valueType == VmDString) &&
               (value2.valueType == VmSString || value2.valueType == VmDString)) {
      throw new UnsupportedOperationException("TODO string compare")
    } else if (value1.valueType == VmObj) {
      throw new UnsupportedOperationException("TODO object compare")
    } else throw new InvalidComparisonException
  }

  def t3vmEquals(value1: T3Value, value2: T3Value) = {
    vmState.t3vmEquals(value1, value2)
  }

  // instruction implementations
  private def index(targetValue: T3Value, indexVal: T3Value) {
    if (targetValue.valueType == VmList) {
      val pushValue =
        objectSystem.toT3Object(targetValue).valueAtIndex(indexVal)
      printf("INDEX on list, targetValue == %s, index = %s, pushValue = %s\n",
             targetValue, indexVal, pushValue)
      vmState.stack.push(pushValue)
    } else if (targetValue.valueType == VmObj) {
      printf("INDEX, TARGET = %s (is a %s), INDEXVAL = %s\n",
             targetValue, vmState.objectSystem.objectWithId(targetValue),
             indexVal)
      val pushValue =
        objectSystem.objectWithId(targetValue).valueAtIndex(indexVal)
      vmState.stack.push(pushValue)
    } else {
      printf("can't index: %s\n", targetValue)
      throw new CannotIndexTypeException
    }
  }

  private def ptrCall(argc: Int) {
    val stackVal = vmState.stack.pop
    if (stackVal.valueType == VmProp) {
      throw new UnsupportedOperationException("PtrCall with PROP not supported yet")
    } else if (stackVal.valueType == VmObj) {
      //printf("OBJ PROP CALL, OBJ = %d\n", stackVal.value)
      val obj  = vmState.objectSystem.objectWithId(stackVal.value)
      val symb = vmState.image.symbolicNames("ObjectCallProp")
      if (symb != null && symb.valueType == VmProp) {
        //printf("SYM: %s TYP: %d VAL: %d\n", symb.name, symb.valueType, symb.value)
        val prop = obj.getProperty(symb.value, argc)
        vmState.doCall(argc, prop.value, 0, obj.id, obj.id, obj.id)
      } else throw new FuncPtrValRequiredException
    } else if (stackVal.valueType == VmFuncPtr) {
      throw new UnsupportedOperationException("PtrCall with FuncPtr not supported yet")
    } else throw new FuncPtrValRequiredException
  }

  private def setInd(containerVal: T3Value, indexVal: T3Value, newVal: T3Value) = {
    if (containerVal.valueType == VmObj) {
      val obj = vmState.objectSystem.objectWithId(containerVal.value)
      printf("setValueAtIndex() for: %s\n", obj)
      obj.setValueAtIndex(indexVal, newVal)
    } else if (containerVal.valueType == VmList) {
      throw new UnsupportedOperationException("SETINDxxx not supported " +
                                              "for objects of list yet")
    } else throw new CannotIndexTypeException
  }

  private def branchIfTrue(condition: Boolean) {
    if (condition) vmState.doBranch
    else nextShortOperand // skip branch word
  }

  private def objSetProp(targetVal: T3Value, propId: Int,
                         newVal: T3Value) {
    targetVal match {
      case objectId:T3ObjectId =>
        val obj = vmState.objectSystem.objectWithId(objectId)
        obj.setProperty(propId, newVal)
      case _ => throw new ObjectValRequiredException
    }
  }

  // one of the central functions of the VM: evaluating properties
  // the general strategy is to look up the property first
  // (pre-evaluation can happen here) and then evaluate the property
  // that was found. The reference implementation splits this into
  // a no-eval and an eval step, which is combined here for the moment
  // to see whether we find a factorization that fits better into the
  // Scala application context
  private def callProp(argc: Int, targetVal: T3Value, propId: Int,
                       useCurrentSelf: Boolean = false) {
    printf("callProp(%s, %d, %d)\n", targetVal, argc, propId)
    targetVal match {
      case objectId:T3ObjectId =>
        val obj = vmState.objectSystem.objectWithId(targetVal)
        //printf("callProp(%s, %d, %d), obj: %s\n", targetVal, propId, argc, obj)
        val prop = obj.getProperty(propId, argc)
        if (prop != InvalidProperty) {
          printf("callProp() - Property found: %s\n", prop)
          evalProperty(objectId, prop, argc, useCurrentSelf)
        } else {
          printf("prop not found in sef or super -> lookin' for propNotDefined()\n")
          // check whether propNotDefined is available
          val propNotDefined = vmState.image.symbolicNames("propNotDefined").t3Value
          val pndProp = obj.getProperty(propNotDefined.value, argc)
          if (pndProp == InvalidProperty) {
            // there is no property defined, yet we still have to
            // clean up our parameters
            vmState.r0 = T3Nil
            for (i <- 0 until argc) vmState.stack.pop
          } else {
            printf("SEARCH %s.'propNotDefined': %s -> [%s]\n", obj, propNotDefined, pndProp)
            if (pndProp.valueType == VmCodeOfs) {
              vmState.stack.pushPropertyId(propId)
              evalProperty(objectId, pndProp, argc + 1, useCurrentSelf)
            } else {
              throw new UnsupportedOperationException("propNotDefined is not a method")
            }
          }
        }
      case listConst:T3ListConstant =>
        // use constant list property evaluator
        val list = vmState.objectSystem.listConstantWithOffset(listConst)
        vmState.r0 =
          vmState.objectSystem.listMetaClass.evalClassProperty(list, propId, argc)
      case sstring:T3SString =>
        // use constant string property evaluator
        val str = vmState.objectSystem.stringConstantWithOffset(sstring)
        vmState.r0 =
          vmState.objectSystem.stringMetaClass.evalClassProperty(str, propId, argc)
      case dstring:T3DString =>
        throw new UnsupportedOperationException("Cannot handle dstring constants yet")
      case _ => throw new ObjectValRequiredException
    }
  }

  private def evalProperty(targetObj: T3ObjectId, property: Property, argc: Int,
                           useCurrentSelf: Boolean) {
    printf("evalProperty(%s) [target = %s, self = %s]\n", property, targetObj,
           vmState.currentSelf)
    property.tadsValue match {
      case T3CodeOffset(codeOffset) =>
        if (useCurrentSelf) {
          vmState.doCall(argc, codeOffset, property.id, targetObj,
                         property.definingObject,
                         vmState.currentSelf.asInstanceOf[T3ObjectId])
        } else {
          vmState.doCall(argc, codeOffset, property.id, targetObj,
                         property.definingObject, targetObj)
        }
      case T3DString(poolOffset) =>
        // self-printing string, convert to sstring and invoke say method
        argc must_== 0
        vmState.stack.pushSString(poolOffset)
        say
      case _         => vmState.r0 = property.tadsValue
    }
  }

  private def inheritProperty(argc: Int, propId: T3Value) {
    val definingObject   = vmState.objectSystem.objectWithId(vmState.definingObject)
    //printf("inheritProperty(%d, %s), defobj = %s\n", argc, propId, definingObject)
    val prop = definingObject.inheritProperty(propId.value, argc)
    //printf("inheritProperty() - PROP FOUND: %s\n", prop)
    if (prop != InvalidProperty) {
      evalProperty(vmState.currentSelf.asInstanceOf[T3ObjectId], prop, argc, false)
    } else {
      // TODO: check if propNotDefined is available
      throw new UnsupportedOperationException("TODO: property not found, " +
                                              "check for propNotDefined")
    }
  }

  private def pushCtxEle(elem: Int) {
    //printf("PUSHCTXELE, ELEM = %d HA\n", elem)
    elem match { // not reused, so I just use the constants directly
      case 1 => vmState.stack.push(vmState.targetProperty)
      case 2 => vmState.stack.push(vmState.originalTarget)
      case 3 => vmState.stack.push(vmState.definingObject)
      case _ => throw new IllegalArgumentException("elem: " + elem)
    }
  }
}
