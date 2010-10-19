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

// * These are the intrinsic function sets, as they are described in the TADS3
// * System Manual, "The Intrinsics".
// * The System Manual seems much more complete than the Technical Manual, and
// * the indexes of the functions are taken from the QTads reference VM.
// * Function sets are indexed in function vectors, so they can be accessed in
// * an implementation-independent way.
// * I'll leave in the index numbers and function names in the comments even
// * though it is redundant information (the order in the array are good enough
// * for me), someone who wants to implement a TADS3 system in another language
// * might find the comments more useful.
abstract class IntrinsicFunctionSet {
  def name: String
  protected var _vmState: TadsVMState        = null
  
  def reset(vmState: TadsVMState) {
    _vmState       = vmState
  }
  def callFunction(argc: Int, functionIndex: Int)
}

// ***********************************************************************
// * t3vm FUNCTION SET
// ***********************************************************************
// * 0: runGC()
// * 1: setSay(funcptr)
// * 2: getVMVsn()
// * 3: getVMID()
// * 4: getVMBanner()
// * 5: getVMPreinitMode()
// * 6: debugTrace(mode, ...)
// * 7: getGlobalSymbols()
// * 8: allocProp()
// * 9: getStackTrace(level?)
// **********************************************************************

class T3VMFunctionSet extends IntrinsicFunctionSet {
  def name = "t3vm"
  var _sayFuncPtr: TadsValue = null

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
    _vmState.r0 = TadsNil // we are never in preinit mode
  }
  private def debugTrace(argc: Int) {
    throw new UnsupportedOperationException("debugTrace() not implemented yet")
  }
  private def getGlobalSymbols(argc: Int) {
    println("t3vm.getGlobalSymbols()")
    _vmState.r0 = TadsNil // TODO: our test game does not have a GSYM
  }
  private def allocProp(argc: Int) {
    throw new UnsupportedOperationException("allocProp() not implemented yet")
  }
  private def getStackTrace(argc: Int) {
    throw new UnsupportedOperationException("getStackTrace() not implemented yet")
  }

  val FunctionVector = Array(
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
    FunctionVector(functionIndex)(this, argc)
  }
}

// ***********************************************************************
// * tads-gen FUNCTION SET
// ***********************************************************************
// * 0: dataType(val)
// * 1: getArg(idx)
// * 2: firstObj(cls?, flags?)
// * 3: nextObj(obj, cls?, flags?)
// * 4: randomize()
// * 5: rand(x, ...)
// * 6: toString(val, radix?)
// * 7: toInteger(val, radix?)
// * 8: getTime(timeType?)
// * 9: rexMatch(pat, str, index?)
// * 10: rexSearch(pat, str, index?)
// * 11: rexGroup(groupNum)
// * 12: rexReplace(pat, str, replacement, flags, index?)
// * 13: savepoint()
// * 14: undo()
// * 15: saveGame(filename)
// * 16: restoreGame(filename)
// * 17: restartGame()
// * 18: getMax(val1, ...)
// * 19: getMin(val1, ...)
// * 20: makeString(val, repeatCount?)
// * 21: getFuncParams(funcptr)
// ***********************************************************************
object TadsGenFunctionSet {
  // enumeration flag values
  val EnumInstances = 1
  val EnumClasses   = 2
}

class TadsGenFunctionSet extends IntrinsicFunctionSet {
  def name = "tads-gen"
  private def dataType(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.dataType() not implemented yet")
  }
  private def getArg(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.getArg() not implemented yet")
  }
  private def firstObj(argc: Int) {
    import TadsGenFunctionSet._
    // set default values for enumeration
    var classId: TadsObjectId = TadsObjectId.InvalidObject
    var flags = EnumInstances

    printf("tads-gen.firstObj(), argc = %d\n", argc)
    // process up to 2 optional parameters (class?, flags?)
    if (argc == 2) {
      classId = _vmState.stack.pop.asInstanceOf[TadsObjectId]
      flags    = _vmState.stack.pop.value
    } else if (argc == 1) {
      val arg = _vmState.stack.pop
      if (arg.valueType == TypeIds.VmInt) {
        flags = arg.value
      } else if (arg.valueType == TypeIds.VmObj) {
        classId = arg.asInstanceOf[TadsObjectId]
      } else {
        throw new IllegalArgumentException("Illegal argument: %s".format(arg))
      }
    }
    printf("CLASS IS: %s, FLAGS IS: %d\n", classId, flags)
    val enumInstances = (flags & EnumInstances) == EnumInstances
    val enumClasses = (flags & EnumClasses) == EnumClasses
    val result = _vmState.objectManager.firstObject(enumInstances, enumClasses,
                                                    classId)
  }
  private def nextObj(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.nextObj() not implemented yet")
  }
  private def randomize(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.randomize() not implemented yet")
  }
  private def rand(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.rand() not implemented yet")
  }
  private def toString(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.toString() not implemented yet")
  }
  private def toInteger(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.toInteger() not implemented yet")
  }
  private def getTime(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.getTime() not implemented yet")
  }
  private def rexMatch(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.rexMatch() not implemented yet")
  }
  private def rexSearch(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.rexSearch() not implemented yet")
  }
  private def rexGroup(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.rexGroup() not implemented yet")
  }
  private def rexReplace(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.rexReplace() not implemented yet")
  }
  private def savepoint(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.savepoint() not implemented yet")
  }
  private def undo(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.undo() not implemented yet")
  }
  private def saveGame(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.saveGame() not implemented yet")
  }
  private def restoreGame(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.restoreGame() not implemented yet")
  }
  private def restartGame(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.restartGame() not implemented yet")
  }
  private def getMax(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.getMax() not implemented yet")
  }
  private def getMin(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.getMin() not implemented yet")
  }
  private def makeString(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.makeString() not implemented yet")
  }
  private def getFuncParams(argc: Int) {
    throw new UnsupportedOperationException("tads-gen.getFuncParams() not " +
                                            "implemented yet")
  }

  val FunctionVector = Array(
    (_: TadsGenFunctionSet).dataType(_: Int),
    (_: TadsGenFunctionSet).getArg(_: Int),
    (_: TadsGenFunctionSet).firstObj(_: Int),
    (_: TadsGenFunctionSet).nextObj(_: Int),
    (_: TadsGenFunctionSet).randomize(_: Int),
    (_: TadsGenFunctionSet).rand(_: Int),
    (_: TadsGenFunctionSet).toString(_: Int),
    (_: TadsGenFunctionSet).toInteger(_: Int),
    (_: TadsGenFunctionSet).getTime(_: Int),
    (_: TadsGenFunctionSet).rexMatch(_: Int),
    (_: TadsGenFunctionSet).rexSearch(_: Int),
    (_: TadsGenFunctionSet).rexGroup(_: Int),
    (_: TadsGenFunctionSet).rexReplace(_: Int),
    (_: TadsGenFunctionSet).savepoint(_: Int),
    (_: TadsGenFunctionSet).undo(_: Int),
    (_: TadsGenFunctionSet).saveGame(_: Int),
    (_: TadsGenFunctionSet).restoreGame(_: Int),
    (_: TadsGenFunctionSet).restartGame(_: Int),
    (_: TadsGenFunctionSet).getMax(_: Int),
    (_: TadsGenFunctionSet).getMin(_: Int),
    (_: TadsGenFunctionSet).makeString(_: Int),
    (_: TadsGenFunctionSet).getFuncParams(_: Int)
  )

  def callFunction(argc: Int, functionIndex: Int) {
    FunctionVector(functionIndex)(this, argc)
  }
}

// ***********************************************************************
// * tads-io FUNCTION SET
// ***********************************************************************
// * 0: tadsSay(val, ...)
// * 1: setLogFile(fnam, logType?)
// * 2: clearScreen()
// * 3: morePrompt()
// * 4: inputLine()
// * 5: inputKey()
// * 6: inputEvent(timeout?)
// * 7: inputDialog(icon, prompt, buttons, defaultButton, cancelButton)
// * 8: inputFile(prompt, dialogType, fileType, flags)
// * 9: timeDelay(delay)
// * 10: systemInfo(infoType, ...)
// * 11: statusMode(mode)
// * 12: statusRight(txt)
// * 13: resExists(resname)
// * 14: setScriptFile(filename, flags?)
// * 15: getLocalCharSet(which)
// * 16: flushOutput()
// * 17: inputLineTimeout(timeout?)
// * 18: inputLineCancel(reset)
// * 19: bannerCreate(parent, where, other, windowType, align, size, sizeUnits, style)
// * 20: bannerDelete(handle)
// * 21: bannerClear(handle)
// * 22: bannerSay(handle, ...)
// * 23: bannerFlush(handle)
// * 24: bannerSizeToContents(handle)
// * 25: bannerGoTo(handle, row, col)
// * 26: bannerSetTextColor(handle, fg, bg)
// * 27: bannerSetScreenColor(handle, color)
// * 28: bannerGetInfo(banner)
// * 29: bannerSetSize(handle, size, sizeUnits, isAdvisory)
// * 30: logConsoleCreate(filename, charset, width)
// * 31: logConsoleClose(handle)
// * 32: logConsoleSay(handle, ...)
// ***********************************************************************
class TadsIoFunctionSet extends IntrinsicFunctionSet {
  def name = "tads-io"
  private def tadsSay(argc: Int) {
    throw new UnsupportedOperationException("tads-io.tadsSay() not implemented yet")
  }
  private def setLogFile(argc: Int) {
    throw new UnsupportedOperationException("tads-io.setLogFile() not implemented yet")
  }
  private def clearScreen(argc: Int) {
    throw new UnsupportedOperationException("tads-io.clearScreen() not implemented yet")
  }
  private def morePrompt(argc: Int) {
    throw new UnsupportedOperationException("tads-io.morePrompt() not implemented yet")
  }
  private def inputLine(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputLine() not implemented yet")
  }
  private def inputKey(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputKey() not implemented yet")
  }
  private def inputEvent(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputEvent() not implemented yet")
  }
  private def inputDialog(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputDialog() not implemented yet")
  }
  private def inputFile(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputFile() not implemented yet")
  }
  private def timeDelay(argc: Int) {
    throw new UnsupportedOperationException("tads-io.timeDelay() not implemented yet")
  }
  private def systemInfo(argc: Int) {
    throw new UnsupportedOperationException("tads-io.systemInfo() not implemented yet")
  }
  private def statusMode(argc: Int) {
    throw new UnsupportedOperationException("tads-io.statusMode() not implemented yet")
  }
  private def statusRight(argc: Int) {
    throw new UnsupportedOperationException("tads-io.statusRight() not implemented yet")
  }
  private def resExists(argc: Int) {
    throw new UnsupportedOperationException("tads-io.resExists() not implemented yet")
  }
  private def setScriptFile(argc: Int) {
    throw new UnsupportedOperationException("tads-io.setScriptFile() not implemented yet")
  }
  private def getLocalCharSet(argc: Int) {
    throw new UnsupportedOperationException("tads-io.getLocalCharSet() not " +
                                            "implemented yet")
  }
  private def flushOutput(argc: Int) {
    throw new UnsupportedOperationException("tads-io.flushOutput() not implemented yet")
  }
  private def inputLineTimeout(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputLineTimeout() " +
                                            "not implemented yet")
  }
  private def inputLineCancel(argc: Int) {
    throw new UnsupportedOperationException("tads-io.inputLineCancel() " +
                                            "not implemented yet")
  }
  private def bannerCreate(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerCreate() not implemented yet")
  }
  private def bannerDelete(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerDelete() not implemented yet")
  }
  private def bannerClear(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerClear() not implemented yet")
  }
  private def bannerSay(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerSay() not implemented yet")
  }
  private def bannerFlush(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerFlush() not implemented yet")
  }
  private def bannerSizeToContents(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerSizeToContents() " +
                                            "not implemented yet")
  }
  private def bannerGoTo(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerGoTo() not implemented yet")
  }
  private def bannerSetTextColor(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerSetTextColor() " +
                                            "not implemented yet")
  }
  private def bannerSetScreenColor(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerSetScreenColor() " +
                                            "not implemented yet")
  }
  private def bannerGetInfo(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerGetInfo() not implemented yet")
  }
  private def bannerSetSize(argc: Int) {
    throw new UnsupportedOperationException("tads-io.bannerSetSize() not implemented yet")
  }
  private def logConsoleCreate(argc: Int) {
    throw new UnsupportedOperationException("tads-io.logConsoleCreate() " +
                                            "not implemented yet")
  }
  private def logConsoleClose(argc: Int) {
    throw new UnsupportedOperationException("tads-io.logConsoleClose() not " +
                                            "implemented yet")
  }
  private def logConsoleSay(argc: Int) {
    throw new UnsupportedOperationException("tads-io.logConsoleSay() not " +
                                            "implemented yet")
  }

  val FunctionVector = Array(
    (_: TadsIoFunctionSet).tadsSay(_: Int),
    (_: TadsIoFunctionSet).setLogFile(_: Int),
    (_: TadsIoFunctionSet).clearScreen(_: Int),
    (_: TadsIoFunctionSet).morePrompt(_: Int),
    (_: TadsIoFunctionSet).inputLine(_: Int),
    (_: TadsIoFunctionSet).inputKey(_: Int),
    (_: TadsIoFunctionSet).inputEvent(_: Int),
    (_: TadsIoFunctionSet).inputDialog(_: Int),
    (_: TadsIoFunctionSet).inputFile(_: Int),
    (_: TadsIoFunctionSet).timeDelay(_: Int),
    (_: TadsIoFunctionSet).systemInfo(_: Int),
    (_: TadsIoFunctionSet).statusMode(_: Int),
    (_: TadsIoFunctionSet).statusRight(_: Int),
    (_: TadsIoFunctionSet).resExists(_: Int),
    (_: TadsIoFunctionSet).setScriptFile(_: Int),
    (_: TadsIoFunctionSet).getLocalCharSet(_: Int),
    (_: TadsIoFunctionSet).flushOutput(_: Int),
    (_: TadsIoFunctionSet).inputLineTimeout(_: Int),
    (_: TadsIoFunctionSet).inputLineCancel(_: Int),
    (_: TadsIoFunctionSet).bannerCreate(_: Int),
    (_: TadsIoFunctionSet).bannerDelete(_: Int),
    (_: TadsIoFunctionSet).bannerClear(_: Int),
    (_: TadsIoFunctionSet).bannerSay(_: Int),
    (_: TadsIoFunctionSet).bannerFlush(_: Int),
    (_: TadsIoFunctionSet).bannerSizeToContents(_: Int),
    (_: TadsIoFunctionSet).bannerGoTo(_: Int),
    (_: TadsIoFunctionSet).bannerSetTextColor(_: Int),
    (_: TadsIoFunctionSet).bannerGetInfo(_: Int),
    (_: TadsIoFunctionSet).bannerSetSize(_: Int),
    (_: TadsIoFunctionSet).logConsoleCreate(_: Int),
    (_: TadsIoFunctionSet).logConsoleClose(_: Int),
    (_: TadsIoFunctionSet).logConsoleSay(_: Int)
  )

  def callFunction(argc: Int, functionIndex: Int) {
    printf("Function Set '%s' callFunction(%d, %d)\n", name, argc, functionIndex)
    FunctionVector(functionIndex)(this, argc)
  }
}

// ***********************************************************************
// * t3test FUNCTION SET
// * This is probably not needed, since it is not described
// * in the System Manual. We won't implement it for now
// ***********************************************************************
// * 0: t3testGetObjId()
// * 1: t3testGetObjGcState()
// * 2: t3testGetCharCode()
// ***********************************************************************
class T3TestFunctionSet extends IntrinsicFunctionSet {
  def name = "t3test"

  private def testGetObjId(argc: Int) {
    throw new UnsupportedOperationException("t3test.testGetObjId() not implemented yet")
  }

  val FunctionVector = Array(
    (_: T3TestFunctionSet).testGetObjId(_: Int)
  )

  def callFunction(argc: Int, functionIndex: Int) {
    printf("Function Set '%s' callFunction(%d, %d)\n", name, argc, functionIndex)
    FunctionVector(functionIndex)(this, argc)
  }
}

// ***********************************************************************
// *
// * 
// ***********************************************************************
class IntrinsicFunctionSetMapper {
  val FunctionSets = Map(
    "t3vm"     -> new T3VMFunctionSet,
    "t3test"   -> new T3TestFunctionSet,
    "tads-gen" -> new TadsGenFunctionSet,
    "tads-io"  -> new TadsIoFunctionSet
  )
  val _functionSets = new Array[IntrinsicFunctionSet](4)

  def reset(vmState: TadsVMState) {
    for (i <- 0 until vmState.image.functionSetDependencies.length) {
      val fsDep = vmState.image.functionSetDependencies(i)
      printf("mapping function set '%s' to index: %d\n",
             fsDep.name, i)
      _functionSets(i) = FunctionSets(fsDep.name)
      _functionSets(i).reset(vmState)
    }
  }

  def callBuiltin(argc: Int, functionIndex: Int, functionSetIndex: Int) {
    _functionSets(functionSetIndex).callFunction(argc, functionIndex)
  }
}
