/*
 * Created on 2010/04/01
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
package org.zmpp.glulx

import scala.annotation.switch
import java.util.logging._
import scala.collection.mutable.HashMap
import org.zmpp.glk.Glk

object AccelFunc {
  var numZRegion = 1
  var numCpTab   = 1
  var numRaPr    = 1
  var numRlPr    = 1
}

abstract class AccelFunc(val _state: GlulxVMState, val _glk: Glk,
                         val _accelParams: Array[Int]) {
  val logger = Logger.getLogger("glulx.accel")
  protected def argIfGiven(args: Array[Int], numArgs: Int, index: Int) = {
    if (index >= numArgs) 0
    else args(index)
  }

  def call(args: Array[Int], numArgs: Int): Int

  // Define functions for inner reuse
  // func 1
  protected def zRegion(addr: Int): Int = {
    if (addr < 36) return 0
    if (addr >= _state.memsize) return 0
    val tb = _state.memByteAt(addr)

    if      (tb >= 0xe0) 3
    else if (tb >= 0xc0) 2
    else if (tb >= 0x70 && tb <= 0x7f && addr >= _state.header.ramstart) 1
    else 0
  }
  // func 2
  def cpTab(obj: Int, id: Int): Int = {
    if (zRegion(obj) != 1) {
      accelError("[** Programming error: tried to find the \".\" of (something) **]")
      return 0
    }
    val otab = _state.memIntAt(obj + 16)
    if (otab == 0) return 0
    val max = _state.memIntAt(otab)
    _state.binarySearch(id, 2, otab + 4, 10, max, 0, 0)
  }
  // func 3
  protected def raPr(obj: Int, id: Int): Int = {
    val prop = getProp(obj, id)
    if (prop == 0) 0
    else _state.memIntAt(prop + 4)
  }
  // func 4
  protected def rlPr(obj: Int, id: Int): Int = {
    val prop = getProp(obj, id)
    if (prop == 0) 0
    else 4 * _state.memShortAt(prop + 2)
  }
  // func 5
  protected def ocCl(obj: Int, cla: Int): Int = {
    val zr = zRegion(obj)
    
    if (zr == 3) {
      return if (cla == StringMetaclass) 1 else 0
    }
    if (zr == 2) {
      return if (cla == RoutineMetaclass) 1 else 0
    }
    if (zr != 1) return 0
    
    if (cla == ClassMetaclass) {
      if (objInClass(obj)) return 1
      if (obj == ClassMetaclass)   return 1
      if (obj == StringMetaclass)  return 1
      if (obj == RoutineMetaclass) return 1
      if (obj == ObjectMetaclass)  return 1
      return 0
    }
    if (cla == ObjectMetaclass) {
      if (objInClass(obj)) return 0
      if (obj == ClassMetaclass)   return 0
      if (obj == StringMetaclass)  return 0
      if (obj == RoutineMetaclass) return 0
      if (obj == ObjectMetaclass)  return 0
      return 1
    }
    if ((cla == StringMetaclass) || (cla == RoutineMetaclass)) return 0
    
    if (!objInClass(cla)) {
      accelError("[** Programming error: tried to apply 'ofclass' with non-class **]")
      return 0
    }
    val prop = getProp(obj, 2)
    if (prop == 0) return 0
    val inlist = _state.memIntAt(prop + 4)
    if (inlist == 0) return 0
    val inlistlen = _state.memShortAt(prop + 2)

    var j = 0
    while (j < inlistlen) {
      if (_state.memIntAt(inlist + 4 * j) == cla) return 1
      j += 1
    }
    0
  }

  // Helpers
  def ClassesTable     = _accelParams(0)
  def IndivPropStart   = _accelParams(1)
  def ClassMetaclass   = _accelParams(2)
  def ObjectMetaclass  = _accelParams(3)
  def RoutineMetaclass = _accelParams(4) 
  def StringMetaclass  = _accelParams(5)
  def Self             = _accelParams(6)
  def NumAttrBytes     = _accelParams(7)
  def CpvStart         = _accelParams(8)
  def Call             = IndivPropStart + 5
  def Print            = IndivPropStart + 6
  def PrintToArray     = IndivPropStart + 7

  def accelError(msg: String) {
    _glk.put_char('\n')
    _glk.put_java_string(msg)
    _glk.put_char('\n')
  }
  def objInClass(obj: Int): Boolean = {
    _state.memIntAt(obj + 13 + NumAttrBytes) == ClassMetaclass
  }

  protected def getProp(anObject: Int, anId: Int): Int = {
    var obj = anObject
    var id  = anId
    var cla = 0

    if ((id & 0xffff0000) != 0) {
      cla = _state.memIntAt(ClassesTable + (id & 0xffff) * 4)
      if (ocCl(obj, cla) == 0) return 0
      id >>= 16
      obj = cla
    }
    val prop = cpTab(obj, id)
    if (prop == 0) return 0
    
    if (objInClass(obj) && cla == 0) {
      if (id < IndivPropStart || id >= IndivPropStart + 8) return 0
    }
    if (_state.memIntAt(Self) != obj) {
      if ((_state.memByteAt(prop + 9) & 1) != 0) return 0
    }
    prop
  }  
}

class Func1ZRegion(state: GlulxVMState, glk:Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int = {
    if (numArgs < 1) 0
    else zRegion(args(0))
  }
}
class Func2CPTab(state: GlulxVMState, glk: Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int =
    cpTab(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1))
}

class Func3RAPr(state: GlulxVMState, glk: Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int =
    raPr(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1))
}

class Func4RLPr(state: GlulxVMState, glk: Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int =
    rlPr(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1))
}
class Func5OCCl(state: GlulxVMState, glk: Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int =
    ocCl(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1))
}

class Func6RVPr(state: GlulxVMState, glk: Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int = {
    val id = argIfGiven(args, numArgs, 1)
    val addr = raPr(args(0), id)
    if (addr == 0) {
      if (id > 0 && id < IndivPropStart) {
        _state.memIntAt(CpvStart + 4 * id)
      } else {
        accelError("[** Programming error: tried to read (something) **]")
        0
      }
    } else _state.memIntAt(addr)
  }
}
class Func7OPPr(state: GlulxVMState, glk: Glk, accelParams: Array[Int])
extends AccelFunc(state, glk, accelParams) {
  def call(args: Array[Int], numArgs: Int): Int = {
    val obj = argIfGiven(args, numArgs, 0)
    val id  = argIfGiven(args, numArgs, 1)
    val zr = zRegion(obj)
    if (zr == 3) {
      return if (id == Print || id == PrintToArray) 1 else 0
    }
    if (zr == 2) {
      return if (id == Call) 1 else 0
    }
    if (zr != 1) return 0
    if (id >= IndivPropStart && id < IndivPropStart + 8) {
      if (objInClass(obj)) return 1
    }
    return if (raPr(obj, id) == 0) 0 else 1
  }
}

object AccelSystem {
  val MaxAccelParams = 9
}

class AccelSystem(vm: GlulxVM) {
  private val logger = Logger.getLogger("glulx.accel")
  private val _accelParams = new Array[Int](AccelSystem.MaxAccelParams)
  private val _accelFunctions = new HashMap[Int, AccelFunc]
  var glk: Glk = null

  private def accelFuncFor(funcnum: Int): AccelFunc = {
    (funcnum: @switch) match {
      case 1 => new Func1ZRegion(vm.state, glk, _accelParams)
      case 2 => new Func2CPTab(vm.state, glk, _accelParams)
      case 3 => new Func3RAPr(vm.state, glk, _accelParams)
      case 4 => new Func4RLPr(vm.state, glk, _accelParams)
      case 5 => new Func5OCCl(vm.state, glk, _accelParams)
      case 6 => new Func6RVPr(vm.state, glk, _accelParams)
      case 7 => new Func7OPPr(vm.state, glk, _accelParams)
      case _ =>
        logger.warning("Unsupported Function number: %d".format(funcnum))
        null
    }
  }
  private def accelFuncForCallAddress(callAddress: Int) = {
    _accelFunctions(callAddress)
  }

  def setParameter(index: Int, value: Int) = {
    if (index >= 0 && index <= 8) _accelParams(index) = value
  }
  def setFunction(accelFuncNum: Int, callAddress: Int) {
    //logger.info("accelfunc #$%02x #$%02x".format(accelFuncNum, callAddress))
    if (accelFuncNum == 0) {
      _accelFunctions.remove(callAddress)
    } else {
      val accelFunc = accelFuncFor(accelFuncNum)
      if (accelFunc != null) _accelFunctions(callAddress) = accelFuncFor(accelFuncNum)
    }
  }
  def isAccelerated(callAddress: Int) = _accelFunctions.contains(callAddress)
  def call(callAddress: Int, args: Array[Int], numArgs: Int) = {
    //logger.info("Function address $%02x is accelerated - REPLACE (TODO)")
    val func = _accelFunctions(callAddress)
    val retval = func.call(args, numArgs)
    vm.popCallStub(retval)
  }
}
