/*
 * Created on 2010/10/09
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

import scala.collection.mutable.HashMap

/*
 * These are the intrinsic function sets, as they are described in the TADS3
 * System Manual, "The Intrinsics".
 * The System Manual seems much more complete than the Technical Manual, and
 * the indexes of the functions are taken from the QTads reference VM.
 * Function sets are indexed in function vectors, so
 * they can be accessed in an implementation-independent way.
 */
abstract class IntrinsicFunctionSet {
  def name: String
  protected var _vmState: Tads3VMState        = null
  protected var _objectManager: ObjectManager = null
  
  def reset(vmState: Tads3VMState, objectManager: ObjectManager) {
    _vmState       = vmState
    _objectManager = _objectManager
  }
  def callFunction(argc: Int, functionIndex: Int)
}

/*
 * t3vm function set.
 * 0: runGC()
 * 1: setSay(funcptr)
 * 2: getVMVsn()
 * 3: getVMID()
 * 4: getVMBanner()
 * 5: getVMPreinitMode()
 * 6: debugTrace(mode, ...)
 * 7: getGlobalSymbols()
 * 8: allocProp()
 * 9: getStackTrace(level?)
 */
class T3VMFunctionSet extends IntrinsicFunctionSet {
  def name = "t3vm"
  var _sayFuncPtr: Tads3Value = null

  private def runGC(argc: Int) { println("t3vm.runGC() [not implemented]") }
  private def setSay(argc: Int) {
    println("t3vm.setSay()")
    if (argc != 1) throw new IllegalArgumentException("setSay() argc must be 1")
    _sayFuncPtr = _vmState.stack.pop
  }
  private def getVMVsn(argc: Int) {
    throw new UnsupportedOperationException("t3vm.getVMVsn() not implemented yet")
  }
  private def getVMID(argc: Int) {
    throw new UnsupportedOperationException("t3vm.getVMID() not implemented yet")
  }
  private def getVMBanner(argc: Int) {
    throw new UnsupportedOperationException("t3vm.getVMBanner() not implemented yet")
  }
  private def getVMPreinitMode(argc: Int) {
    println("t3vm.getPreinitMode()")
    _vmState.r0 = Tads3Nil // we are never in preinit mode
  }
  private def debugTrace(argc: Int) {
    throw new UnsupportedOperationException("debugTrace() not implemented yet")
  }
  private def getGlobalSymbols(argc: Int) {
    println("t3vm.getGlobalSymbols()")
    _vmState.r0 = Tads3Nil // TODO: our test game does not have a GSYM
  }
  private def allocProp(argc: Int) {
    throw new UnsupportedOperationException("allocProp() not implemented yet")
  }
  private def getStackTrace(argc: Int) {
    throw new UnsupportedOperationException("getStackTrace() not implemented yet")
  }

  val FuncVector = Array(
    (_: T3VMFunctionSet).runGC(_: Int),
    (_: T3VMFunctionSet).setSay(_: Int),
    (_: T3VMFunctionSet).getVMVsn(_: Int),
    (_: T3VMFunctionSet).getVMID(_: Int),
    (_: T3VMFunctionSet).getVMBanner(_: Int),
    (_: T3VMFunctionSet).getVMPreinitMode(_: Int),
    (_: T3VMFunctionSet).debugTrace(_: Int),
    (_: T3VMFunctionSet).getGlobalSymbols(_: Int),
    (_: T3VMFunctionSet).allocProp(_: Int),
    (_: T3VMFunctionSet).getStackTrace(_: Int)
  )

  def callFunction(argc: Int, functionIndex: Int) {
    FuncVector(functionIndex)(this, argc)
  }
}

/*
 * tads-get function set.
 * 0: dataType(val)
 * 1: getArg(idx)
 * 2: firstObj(cls?, flags?)
 * 3: nextObj(obj, cls?, flags?)
 * 4: randomize()
 * 5: rand(x, ...)
 * 6: toString(val, radix?)
 * 7: toInteger(val, radix?)
 * 8: getTime(timeType?)
 * 9: rexMatch(pat, str, index?)
 * 10: rexSearch(pat, str, index?)
 * 11: rexGroup(groupNum)
 * 12: rexReplace(pat, str, replacement, flags, index?)
 * 13: savepoint()
 * 14: undo()
 * 15: saveGame(filename)
 * 16: restoreGame(filename)
 * 17: restartGame()
 * 18: getMax(val1, ...)
 * 19: getMin(val1, ...)
 * 20: makeString(val, repeatCount?)
 * 21: getFuncParams(funcptr)
 */
class TadsGenFunctionSet extends IntrinsicFunctionSet {
  def name = "tads-gen"
  def callFunction(argc: Int, functionIndex: Int) {
    printf("Function Set '%s' callFunction(%d, %d)\n", name, argc, functionIndex)
    throw new UnsupportedOperationException("BUILTIN_B not supported yet")
  }
}

/*
 * tads-io function set
 * 0: tadsSay(val, ...)
 * 1: TODO
 */
class TadsIoFunctionSet extends IntrinsicFunctionSet {
  def name = "tads-io"
  def callFunction(argc: Int, functionIndex: Int) {
    printf("Function Set '%s' callFunction(%d, %d)\n", name, argc, functionIndex)
  }
}

/*
 * t3test function set. This is probably not needed, since it is not described
 * in the System Manual. We won't implement it for now
 * 0: t3testGetObjId()
 * 1: t3testGetObjGcState()
 * 2: t3testGetCharCode()
 */
class T3TestFunctionSet extends IntrinsicFunctionSet {
  def name = "t3test"
  def callFunction(argc: Int, functionIndex: Int) {
    printf("Function Set '%s' callFunction(%d, %d)\n", name, argc, functionIndex)
  }
}

class IntrinsicFunctionSetMapper {
  val FunctionSets = Map(
    "t3vm"     -> new T3VMFunctionSet,
    "t3test"   -> new T3TestFunctionSet,
    "tads-gen" -> new TadsGenFunctionSet,
    "tads-io"  -> new TadsIoFunctionSet
  )
  val _functionSets = new Array[IntrinsicFunctionSet](4)

  def reset(vmState: Tads3VMState, objectManager: ObjectManager) {
    for (i <- 0 until vmState.image.functionSetDependencies.length) {
      val fsDep = vmState.image.functionSetDependencies(i)
      printf("mapping function set '%s' to index: %d\n",
             fsDep.name, i)
      _functionSets(i) = FunctionSets(fsDep.name)
      _functionSets(i).reset(vmState, objectManager)
    }
  }

  def callBuiltin(argc: Int, functionIndex: Int, functionSetIndex: Int) {
    _functionSets(functionSetIndex).callFunction(argc, functionIndex)
  }
}
