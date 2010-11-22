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

object TadsConstants {
  val SizePropertyId = 2
  // A data holder is defined in TADS3 as a prefix byte (specifying the type)
  // and a 4-byte value
  val SizeDataHolder = 5
}

/***********************************************************************
 * Definition of the constants of type TadsValue. These are the values
 * that appear on the TADS3 VM stack or in register R0.
 * We define a uniform access interface, each object's value is based
 * on an integer. Values that do not have an integer representation
 * (e.g. Nil, True, Empty...) just return 0 as their value because their
 * value attribute is of no interest (they are interesting only as the
 * constants they represent). Values that have an underlying object return
 * an integer value as well, which however is an offset into a constant
 * pool.
 */
abstract class T3Value {
  def isTrue = true
  def valueType: Int
  def value = 0

  override def equals(obj: Any): Boolean = {
    obj match {
      case other:T3Value =>
        valueType == other.valueType && value == other.value
      case _ => false
    }
  }
  // equals method under VM rules. These are according to the
  // EQ instruction. The default is identical to the regular equals()
  def t3vmEquals(other: T3Value): Boolean = equals(other)
}

object T3Nil extends T3Value {
  override def isTrue = false
  def valueType = TypeIds.VmNil
  override def toString = "NIL"
}
object T3True extends T3Value {
  def valueType = TypeIds.VmTrue
  override def value = 1
  override def toString = "TRUE"
}
object T3Empty extends T3Value {
  def valueType = TypeIds.VmEmpty
  override def toString = "EMPTY"
  override def t3vmEquals(other: T3Value): Boolean = false
}
class T3ListConstant(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmList
  override def toString = "list (offset = %d)".format(value)
  override def t3vmEquals(other: T3Value): Boolean = {
    throw new UnsupportedOperationException("not implemented yet")
  }
}
class T3PropertyId(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmProp
  override def toString = "property (value = %d)".format(value)
}
class T3ObjectId(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmObj
  override def toString = "objectid (value = %d)".format(value)
  override def t3vmEquals(other: T3Value): Boolean = {
    throw new UnsupportedOperationException("not implemented yet")
  }
}
class T3CodeOffset(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmCodeOfs
  override def toString = "code-offset (value = %d)".format(value)
}
class T3Integer(override val value: Int) extends T3Value {
  override def isTrue = value != 0
  def valueType = TypeIds.VmInt
  override def toString = "integer (value = %d)".format(value)
}
class T3FunctionPointer(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmFuncPtr
  override def toString = "function-ptr (value = %d)".format(value)
}
class T3StackRef(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmStack
  override def toString = "stack (value = %d)".format(value)
}
class T3SString(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmSString
  override def toString = "sstring (value = %d)".format(value)
}
class T3DString(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmDString
  override def toString = "dstring (value = %d)".format(value)
  override def t3vmEquals(other: T3Value): Boolean = {
    throw new UnsupportedOperationException("not implemented yet")
  }
}
class T3Enum(override val value: Int) extends T3Value {
  def valueType = TypeIds.VmEnum
  override def toString = "enum (value = %d)".format(value)
}

object T3Integer {
  val Zero = new T3Integer(0)
  val One  = new T3Integer(1)
}
object InvalidObjectId extends T3ObjectId(0)
object InvalidPropertyId extends T3PropertyId(0)
object T3Value {
  def create(valueType: Int, value: Int): T3Value = {
    import TypeIds._
    valueType match {
      case VmNil     => T3Nil
      case VmTrue    => T3True
      case VmStack   => new T3StackRef(value)
      case VmObj     => new T3ObjectId(value)
      case VmProp    => new T3PropertyId(value)
      case VmInt     => new T3Integer(value)
      case VmSString => new T3SString(value)
      case VmDString => new T3DString(value)
      case VmList    => new T3ListConstant(value)
      case VmCodeOfs => new T3CodeOffset(value)
      case VmFuncPtr => new T3FunctionPointer(value)
      case VmEmpty   => T3Empty
      case VmEnum    => new T3Enum(value)
      case _         => throw new IllegalArgumentException("illegal value type: "
                                                           + valueType)
    }
  }
}

class Stack {

  var _stack = new Array[T3Value](300)
  var sp = 0

  def size = _stack.length
  def pushNil = push(T3Nil)
  def pushPropertyId(id: Int) = push(new T3PropertyId(id))
  def pushObjectId(id: Int) = push(new T3ObjectId(id))
  def pushCodeOffset(offset: Int) = push(new T3CodeOffset(offset))
  def pushFunctionPointer(offset: Int) = push(new T3FunctionPointer(offset))
  def pushInt(value: Int) = push(new T3Integer(value))
  def pushStackRef(value: Int) = push(new T3StackRef(value))
  def push0 = push(T3Integer.Zero)
  def push1 = push(T3Integer.One)

  def push(value: T3Value) = {
    _stack(sp) = value
    sp += 1
  }
  def pop: T3Value = {
    sp -= 1
    _stack(sp)
  }
  def top = _stack(sp - 1)
  def dup = push(top)
  def valueAt(index: Int) = _stack(index)
  def setValueAt(index: Int, value: T3Value) = _stack(index) = value

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
  val Push0           = 0x01
  val Push1           = 0x02
  val PushInt8        = 0x03
  val PushInt         = 0x04
  val PushStr         = 0x05
  val PushLst         = 0x06
  val PushObj         = 0x07
  val PushNil         = 0x08
  val PushTrue        = 0x09
  val PushFnPtr       = 0x0b
  val Eq              = 0x40
  val Ne              = 0x41
  val Lt              = 0x42
  val Le              = 0x43
  val Gt              = 0x44
  val Ge              = 0x45
  val RetVal          = 0x50
  val RetNil          = 0x51
  val RetTrue         = 0x52
  val Ret             = 0x54
  val Call            = 0x58
  val PtrCall         = 0x59
  val GetProp         = 0x60
  val CallProp        = 0x61
  val PtrCallProp     = 0x62
  val GetPropSelf     = 0x63
  val CallPropSelf    = 0x64
  val PtrCallPropSelf = 0x65
  val ObjGetProp      = 0x66
  val ObjCallProp     = 0x67
  val GetPropLcl1     = 0x6a
  val GetPropR0       = 0x6c
  val CallPropR0      = 0x6d
  val PtrInherit      = 0x73
  val GetLcl1         = 0x80
  val GetArg1         = 0x82
  val GetArg2         = 0x83
  val PushSelf        = 0x84
  val Dup             = 0x88
  val GetR0           = 0x8b
  val GetDbArgc       = 0x8c
  val Swap            = 0x8d
  val PushCtxEle      = 0x8e
  val Switch          = 0x90
  val Jmp             = 0x91
  val Jt              = 0x92
  val Jf              = 0x93
  val Je              = 0x94
  val Jne             = 0x95
  val Jgt             = 0x96
  val Jge             = 0x97
  val Jlt             = 0x98
  val Jle             = 0x99
  val JNil            = 0x9e
  val JNotNil         = 0x9f
  val JR0T            = 0xa0
  val JR0F            = 0xa1
  val BuiltinA        = 0xb1
  val BuiltinB        = 0xb2
  val BuiltinC        = 0xb3
  val BuiltinD        = 0xb4
  val Builtin1        = 0xb5
  val Builtin2        = 0xb6
  val IdxLcl1Int8     = 0xbb
  val IdxInt8         = 0xbc
  val New1            = 0xc0
  val New2            = 0xc1
  val TrNew1          = 0xc2
  val TrNew2          = 0xc3
  val OneLcl1         = 0xda
  val SetLcl1         = 0xe0
  val SetInd          = 0xe4
  val SetProp         = 0xe5
  val PtrSetProp      = 0xe6
  val SetPropSelf     = 0xe7
  val ObjSetProp      = 0xe8
  val SetSelf         = 0xeb
  val SetLcl1R0       = 0xee
  val SetIndLcl1I8    = 0xef
  val BP              = 0xf1
  val Nop             = 0xf2
}

object OpcodeNames {
  import Opcodes._
  val Names = Map(
    BP              -> "BP",
    Builtin1        -> "BUILTIN1",
    Builtin2        -> "BUILTIN2",
    BuiltinA        -> "BUILTIN_A",
    BuiltinB        -> "BUILTIN_B",
    BuiltinC        -> "BUILTIN_C",
    BuiltinD        -> "BUILTIN_D",
    Call            -> "CALL",
    CallProp        -> "CALLPROP",
    CallPropR0      -> "CALLPROPR0",
    CallPropSelf    -> "CALLPROPSELF",
    Dup             -> "DUP",
    Eq              -> "EQ",
    Ge              -> "GE",
    GetArg1         -> "GETARG1",
    GetArg2         -> "GETARG2",
    GetDbArgc       -> "GETDBARGC",
    GetLcl1         -> "GETLCL1",
    GetProp         -> "GETPROP",
    GetPropLcl1     -> "GETPROPLCL1",
    GetPropR0       -> "GETPROPR0",
    GetPropSelf     -> "GETPROPSELF",
    GetR0           -> "GETR0",
    Gt              -> "GT",
    IdxInt8         -> "IDXINT8",
    IdxLcl1Int8     -> "IDXLCL1INT8",
    Je              -> "JE",
    Jf              -> "JF",
    Jge             -> "JGE",
    Jgt             -> "JGT",
    Jle             -> "JLE",
    Jlt             -> "JLT",
    Jmp             -> "JMP",
    Jne             -> "JNE",
    JNil            -> "JNIL",
    JNotNil         -> "JNOTNIL",
    JR0T            -> "JR0T",
    JR0F            -> "JR0F",
    Jt              -> "JT",
    Le              -> "LE",
    Lt              -> "LT",
    Ne              -> "NE",
    New1            -> "NEW1",
    New2            -> "NEW2",
    Nop             -> "NOP",
    ObjCallProp     -> "OBJCALLPROP",
    ObjGetProp      -> "OBJGETPROP",
    ObjSetProp      -> "OBJSETPROP",
    OneLcl1         -> "ONELCL1",
    PtrCall         -> "PTRCALL",
    PtrCallProp     -> "PTRCALLPROP",
    Push0           -> "PUSH_0",
    Push1           -> "PUSH_1",
    PushCtxEle      -> "PUSHCTXELE",
    PushFnPtr       -> "PUSHFNPTR",
    PushInt         -> "PUSHINT",
    PushInt8        -> "PUSHINT8",
    PushLst         -> "PUSHLST",
    PushNil         -> "PUSHNIL",
    PushObj         -> "PUSHOBJ",
    PushSelf        -> "PUSHSELF",
    PushStr         -> "PUSHSTR",
    PushTrue        -> "PUSHTRUE",
    PtrCallPropSelf -> "PTRCALLPROPSELF",
    PtrInherit      -> "PTRINHERIT",
    PtrSetProp      -> "PTRSETPROP",
    Ret             -> "RET",
    RetNil          -> "RETNIL",
    RetTrue         -> "RETTRUE",
    RetVal          -> "RETVAL",
    SetInd          -> "SETIND",
    SetIndLcl1I8    -> "SETINDLCL1I8",
    SetLcl1         -> "SETLCL1",
    SetLcl1R0       -> "SETLCL1R0",
    SetProp         -> "SETPROP",
    SetPropSelf     -> "SETPROPSELF",
    SetSelf         -> "SETSELF",
    Swap            -> "SWAP",
    Switch          -> "SWITCH",
    TrNew1          -> "TRNEW1",
    TrNew2          -> "TRNEW2"
  )
  def opcodeName(opcodeNum: Int) = {
    if (Names.contains(opcodeNum)) Names(opcodeNum)
    else "??? (%02x)".format(opcodeNum)
  }
}


// Exceptions
class CannotIndexTypeException extends Exception
class ObjectNotFoundException extends Exception
class FuncPtrValRequiredException extends Exception
class ObjectValRequiredException extends Exception
class InvalidComparisonException extends Exception
