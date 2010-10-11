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

/*
 * These type ids and their names are taken from the "T3 Portable Binary Encoding"
 * section.
 * ZMPP is unlikely to use any native reference values (VM_STACK, VM_CODEPTR,
 * VM_NATIVE_CODE) and the related constants here are only listed for completeness.
 */
object TypeIds {
  val VmNil        = 1  // null and false representation
  val VmTrue       = 2  // true representation
  val VmStack      = 3  // native reference to the stack
  val VmCodePtr    = 4  // native code pointer (unused in ZMPP)
  val VmObj        = 5  // object id
  val VmProp       = 6  // property id
  val VmInt        = 7  // integer value
  val VmSString    = 8  // single quoted string
  val VmDString    = 9  // double quoted string
  val VmList       = 10 // list constant
  val VmCodeOfs    = 11 // code offset
  val VmFuncPtr    = 12 // function pointer
  val VmEmpty      = 13 // empty value (for initializations)
  val VmNativeCode = 14 // native code ptr (unused in ZMPP)
  val VmEnum       = 15 // enumerated constant

  // property values are truncated to unsigned 16 bit
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

/***********************************************************************
 * Definition of the constants of type Tads3Value. These are the values
 * that appear on the TADS3 VM stack or in register R0.
 * We define a uniform access interface, each object's value is based
 * on an integer. Values that do not have an integer representation
 * (e.g. Nil, True, Empty...) just return 0 as their value because their
 * value attribute is of no interest (they are interesting only as the
 * constants they represent). Values that have an underlying object return
 * an integer value as well, which however is an offset into a constant
 * pool.
 */
abstract class Tads3Value {
  def isTrue = true
  def valueType: Int
  def value = 0
}

object Tads3Nil extends Tads3Value {
  override def isTrue = false
  def valueType = TypeIds.VmNil
  override def toString = "NIL"
}
object Tads3True extends Tads3Value {
  def valueType = TypeIds.VmTrue
  override def value = 1
  override def toString = "TRUE"
}
object Tads3Empty extends Tads3Value {
  def valueType = TypeIds.VmEmpty
  override def toString = "EMPTY"
}

class Tads3ListConstant extends Tads3Value {
  def valueType = TypeIds.VmList
  override def toString = "list"
}
class Tads3PropertyId(override val value: Int) extends Tads3Value {
  def valueType = TypeIds.VmProp
  override def toString = "property (value = %d)".format(value)
}

object Tads3ObjectId {
  val InvalidObject = new Tads3ObjectId(0)
}
class Tads3ObjectId(override val value: Int) extends Tads3Value {
  def valueType = TypeIds.VmObj
  override def toString = "objectid (value = %d)".format(value)
}
class Tads3CodeOffset(override val value: Int) extends Tads3Value {
  def valueType = TypeIds.VmCodeOfs
  override def toString = "code-offset (value = %d)".format(value)
}

object Tads3Integer {
  val One = new Tads3Integer(1)
}
class Tads3Integer(override val value: Int) extends Tads3Value {
  override def isTrue = value != 0
  def valueType = TypeIds.VmInt
  override def toString = "integer (value = %d)".format(value)
}

class Tads3FunctionPointer(override val value: Int) extends Tads3Value {
  def valueType = TypeIds.VmFuncPtr
  override def toString = "function-ptr (value = %d)".format(value)
}

class Tads3StackRef(override val value: Int) extends Tads3Value {
  def valueType = TypeIds.VmStack
  override def toString = "stack (value = %d)".format(value)
}

class Tads3Stack {

  var _stack = new Array[Tads3Value](300)
  var sp = 0

  def size = _stack.length
  def pushNil = push(Tads3Nil)

  def pushPropertyId(id: Int) = push(new Tads3PropertyId(id))
  def pushObjectId(id: Int) = push(new Tads3ObjectId(id))
  def pushCodeOffset(offset: Int) = push(new Tads3CodeOffset(offset))
  def pushFunctionPointer(offset: Int) = push(new Tads3FunctionPointer(offset))
  def pushInt(value: Int) = push(new Tads3Integer(value))
  def pushStackRef(value: Int) = push(new Tads3StackRef(value))
  def push1 = push(Tads3Integer.One)

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

  override def toString = {
    val buffer = new StringBuilder
    println("-----------------\n")
    buffer.append("STACK:\n")
    for (i <- 0 until sp) {
      buffer.append("%d: ".format(i))
      buffer.append(_stack(i))
      buffer.append("\n")
    }
    buffer.append("-----------------\n")
    buffer.toString
  }
}

// The machine opcodes
object Opcodes {
  val Push1        = 0x02
  val PushNil      = 0x08
  val PushFnPtr    = 0x0b
  val RetNil       = 0x51
  val Call         = 0x58
  val GetPropSelf  = 0x63
  val ObjGetProp   = 0x66
  val GetLcl1      = 0x80
  val GetArg1      = 0x82
  val PushSelf     = 0x84
  val Dup          = 0x88
  val GetR0        = 0x8b
  val JNil         = 0x9e
  val JR0T         = 0xa0
  val BuiltinA     = 0xb1
  val BuiltinB     = 0xb2
  val BuiltinC     = 0xb3
  val BuiltinD     = 0xb4
  val Builtin1     = 0xb5
  val Builtin2     = 0xb6
  val New1         = 0xc0
  val SetLcl1      = 0xe0
  val SetInd       = 0xe4
  val SetLcl1R0    = 0xee
  val SetIndLcl1I8 = 0xef
  val BP           = 0xf1
  val Nop          = 0xf2
}

object OpcodeNames {
  import Opcodes._
  val Names = Map(
    BP           -> "BP",
    Builtin1     -> "BUILTIN1",
    Builtin2     -> "BUILTIN2",
    BuiltinA     -> "BUILTIN_A",
    BuiltinB     -> "BUILTIN_B",
    BuiltinC     -> "BUILTIN_C",
    BuiltinD     -> "BUILTIN_D",
    Call         -> "CALL",
    Dup          -> "DUP",
    GetArg1      -> "GETARG1",
    GetLcl1      -> "GETLCL1",
    GetPropSelf  -> "GETPROPSELF",
    GetR0        -> "GETR0",
    JNil         -> "JNIL",
    JR0T         -> "JR0T",
    New1         -> "NEW1",
    Nop          -> "NOP",
    ObjGetProp   -> "OBJGETPROP",
    Push1        -> "PUSH_1",
    PushNil      -> "PUSHNIL",
    PushFnPtr    -> "PUSHFNPTR",
    PushSelf     -> "PUSHSELF",
    RetNil       -> "RETNIL",
    SetInd       -> "SETIND",
    SetIndLcl1I8 -> "SETINDLCL1I8",
    SetLcl1      -> "SETLCL1",
    SetLcl1R0    -> "SETLCL1R0"
  )
  def opcodeName(opcodeNum: Int) = {
    if (Names.contains(opcodeNum)) Names(opcodeNum)
    else "??? (%02x)".format(opcodeNum)
  }
}


// Exceptions
class CannotIndexTypeException extends Exception
