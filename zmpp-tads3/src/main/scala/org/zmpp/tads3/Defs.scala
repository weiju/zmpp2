/*
 * Created on 2010/05/08
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

import scala.collection.mutable.ArrayStack

object TypeIds {
  val VmNil        = 1
  val VmTrue       = 2
  val VmStack      = 3
  val VmCodePtr    = 4
  val VmObj        = 5
  val VmProp       = 6
  val VmInt        = 7
  val VmSString    = 8
  val VmDString    = 9
  val VmList       = 10
  val VmCodeOfs    = 11
  val VmFuncPtr    = 12
  val VmEmpty      = 13
  val VmNativeCode = 14
  val VmEnum       = 15

  def valueForType(typ: Int, value: Int) = {
    if (typ == TypeIds.VmProp) value & 0xffff else value
  }
}

object Tads3Constants {
  val SizePropertyId = 2
  // A data holder is defined in TADS3 as a prefix byte (specifying the type)
  // and a 4-byte value
  val SizeDataHolder = 5
}

/*
 * Values that can appear in the TADS3 stack or in register R0
 */
abstract class Tads3Value {
  def isTrue = true
  def valueType: Int
}

// Definition of the constants of type Tads3Value
object Tads3Nil extends Tads3Value {
  override def isTrue = false
  def valueType = TypeIds.VmNil
}
object Tads3True extends Tads3Value {
  def valueType = TypeIds.VmTrue
}
object Tads3Empty extends Tads3Value {
  def valueType = TypeIds.VmEmpty
}
class Tads3List extends Tads3Value {
  def valueType = TypeIds.VmList
}
class Tads3PropertyId(val id: Int) extends Tads3Value {
  def valueType = TypeIds.VmProp
}
class Tads3ObjectId(val id: Int) extends Tads3Value {
  def valueType = TypeIds.VmObj
}
class Tads3CodeOffset(val offset: Int) extends Tads3Value {
  def valueType = TypeIds.VmCodeOfs
}
class Tads3Integer(val value: Int) extends Tads3Value {
  override def isTrue = value != 0
  def valueType = TypeIds.VmInt
}
class Tads3FunctionPointer(val offset: Int) extends Tads3Value {
  def valueType = TypeIds.VmFuncPtr
}

class Tads3Stack {
  // do not create too many unnecessary one's
  val IntegerOne = new Tads3Integer(1)

  var _stack = new Array[Tads3Value](300)
  var sp = 0

  def size = _stack.length
  def pushNil = push(Tads3Nil)

  def pushPropertyId(id: Int) = push(new Tads3PropertyId(id))
  def pushObjectId(id: Int) = push(new Tads3ObjectId(id))
  def pushCodeOffset(offset: Int) = push(new Tads3CodeOffset(offset))
  def pushFunctionPointer(offset: Int) = push(new Tads3FunctionPointer(offset))
  def pushInt(value: Int) = push(new Tads3Integer(value))
  def push1 = push(IntegerOne)

  def push(value: Tads3Value) = {
    _stack(sp) = value
    sp += 1
  }
  def pop: Tads3Value = {
    sp -= 1
    _stack(sp)
  }
  def top = _stack(sp - 1)
  def dup = push(top)
  def valueAt(index: Int) = _stack(index)
  def setValueAt(index: Int, value: Tads3Value) = _stack(index) = value
}

// The machine opcodes
object Opcodes {
  val Push1      = 0x02
  val PushNil    = 0x08
  val PushFnPtr  = 0x0b
  val RetNil     = 0x51
  val Call       = 0x58
  val ObjGetProp = 0x66
  val GetArg1    = 0x82
  val Dup        = 0x88
  val GetR0      = 0x8b
  val JNil       = 0x9e
  val JR0T       = 0xa0
  val BuiltinA   = 0xb1
  val BuiltinB   = 0xb2
  val BuiltinC   = 0xb3
  val BuiltinD   = 0xb4
  val New1       = 0xc0
  val SetLcl1    = 0xe0
}

object OpcodeNames {
  import Opcodes._
  val Names = Map(
    BuiltinA   -> "BUILTIN_A",
    BuiltinB   -> "BUILTIN_B",
    BuiltinC   -> "BUILTIN_C",
    BuiltinD   -> "BUILTIN_D",
    Call       -> "CALL",
    Dup        -> "DUP",
    GetArg1    -> "GETARG1",
    GetR0      -> "GETR0",
    JNil       -> "JNIL",
    JR0T       -> "JR0T",
    New1       -> "NEW1",
    ObjGetProp -> "OBJGETPROP",
    Push1      -> "PUSH_1",
    PushNil    -> "PUSHNIL",
    PushFnPtr  -> "PUSHFNPTR",
    RetNil     -> "RETNIL",
    SetLcl1    -> "SETLCL1"
  )
  def opcodeName(opcodeNum: Int) = {
    if (Names.contains(opcodeNum)) Names(opcodeNum)
    else "??? (%02x)".format(opcodeNum)
  }
}


