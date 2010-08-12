/*
 * Created on 2010/04/01
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
package org.zmpp.glulx

import java.util.logging._
import org.zmpp.base.Memory

// *************************************************************************
// ***** Glulx Definitions
// ***** Rather than defining enumerations for everything, we define
// ***** frequently used constants as simple int values
// *************************************************************************

object AddressModes {
  val ConstZero        = 0
  val ConstByte        = 1
  val ConstShort       = 2
  val ConstInt         = 3
  val Address00_FF     = 5
  val Address0000_FFFF = 6
  val AddressAny       = 7
  val Stack            = 8
  val Local00_FF       = 9
  val Local0000_FFFF   = 10
  val LocalAny         = 11
  val Ram00_FF         = 13
  val Ram0000_FFFF     = 14
  val RamAny           = 15  
}

object DestTypes {
  val DoNotStore    = 0
  val Memory        = 1
  val LocalVariable = 2
  val Stack         = 3
  val Ram           = 4
  
  // while printing
  val ResumePrintCompressed = 10
  val ResumeExecuteFunction = 11
  val ResumePrintDecimal    = 12
  val ResumePrintCString    = 13
  val ResumePrintUnicode    = 14
  def isStringDestType(destType: Int) = destType >= 10

  def fromAddressMode(addressMode: Int) = addressMode match {
    case AddressModes.ConstZero        => DestTypes.DoNotStore
    case AddressModes.Address00_FF     => DestTypes.Memory
    case AddressModes.Address0000_FFFF => DestTypes.Memory
    case AddressModes.AddressAny       => DestTypes.Memory
    case AddressModes.Stack            => DestTypes.Stack
    case AddressModes.Local00_FF       => DestTypes.LocalVariable
    case AddressModes.Local0000_FFFF   => DestTypes.LocalVariable
    case AddressModes.LocalAny         => DestTypes.LocalVariable
    case AddressModes.Ram00_FF         => DestTypes.Ram
    case AddressModes.Ram0000_FFFF     => DestTypes.Ram
    case AddressModes.RamAny           => DestTypes.Ram
    case _ =>
      throw new IllegalArgumentException(
        "unknown address mode: %02x".format(addressMode))
  }  
}

object Opcodes {
  val Nop           = 0x00
  val Add           = 0x10
  val Sub           = 0x11
  val Mul           = 0x12
  val Div           = 0x13
  val Mod           = 0x14
  val Neg           = 0x15
  val Bitand        = 0x18
  val Bitor         = 0x19
  val Bitxor        = 0x1a
  val Bitnot        = 0x1b
  val ShiftL        = 0x1c
  val SShiftR       = 0x1d
  val UShiftR       = 0x1e
  val Jump          = 0x20
  val Jz            = 0x22
  val Jnz           = 0x23
  val Jeq           = 0x24
  val Jne           = 0x25
  val Jlt           = 0x26
  val Jge           = 0x27
  val Jgt           = 0x28
  val Jle           = 0x29
  val Jltu          = 0x2a
  val Jgeu          = 0x2b
  val Jgtu          = 0x2c
  val Jleu          = 0x2d
  val Call          = 0x30
  val Return        = 0x31
  val Catch         = 0x32
  val Throw         = 0x33
  val TailCall      = 0x34
  val Copy          = 0x40
  val Copys         = 0x41
  val Copyb         = 0x42
  val Sexs          = 0x44
  val Sexb          = 0x45
  val Aload         = 0x48
  val Aloads        = 0x49
  val Aloadb        = 0x4a
  val AloadBit      = 0x4b
  val Astore        = 0x4c
  val Astores       = 0x4d
  val Astoreb       = 0x4e
  val AstoreBit     = 0x4f
  val StkCount      = 0x50
  val StkPeek       = 0x51
  val StkSwap       = 0x52
  val StkRoll       = 0x53
  val StkCopy       = 0x54
  val StreamChar    = 0x70
  val StreamNum     = 0x71
  val StreamStr     = 0x72
  val StreamUniChar = 0x73
  val Gestalt       = 0x100
  val DebugTrap     = 0x101
  val GetMemSize    = 0x102
  val SetMemSize    = 0x103
  val JumpAbs       = 0x104
  val Random        = 0x110
  val SetRandom     = 0x111
  val Quit          = 0x120
  val Verify        = 0x121
  val Restart       = 0x122
  val Save          = 0x123
  val Restore       = 0x124
  val SaveUndo      = 0x125
  val RestoreUndo   = 0x126
  val Protect       = 0x127
  val Glk           = 0x130
  val GetStringTbl  = 0x140
  val SetStringTbl  = 0x141
  val GetIOSys      = 0x148
  val SetIOSys      = 0x149
  val LinearSearch  = 0x150
  val BinarySearch  = 0x151
  val LinkedSearch  = 0x152
  val Callf         = 0x160
  val Callfi        = 0x161
  val Callfii       = 0x162
  val Callfiii      = 0x163
  val Mzero         = 0x170
  val Mcopy         = 0x171
  val Malloc        = 0x178
  val Mfree         = 0x179
  val AccelFunc     = 0x180
  val AccelParam    = 0x181

  val NumToF        = 0x190
  val FtoNumZ       = 0x191
  val FtoNumN       = 0x192
  val Ceil          = 0x198
  val Floor         = 0x199
  val Fadd          = 0x1a0
  val Fsub          = 0x1a1
  val Fmul          = 0x1a2
  val Fdiv          = 0x1a3
  val Fmod          = 0x1a4
  val Sqrt          = 0x1a8
  val Exp           = 0x1a9
  val Log           = 0x1aa
  val Pow           = 0x1ab
  val Sin           = 0x1b0
  val Cos           = 0x1b1
  val Tan           = 0x1b2
  val Asin          = 0x1b3
  val Acos          = 0x1b4
  val Atan          = 0x1b5
  val Atan2         = 0x1b6
  val Jfeq          = 0x1c0
  val Jfne          = 0x1c1
  val Jflt          = 0x1c2
  val Jfle          = 0x1c3
  val Jfgt          = 0x1c4
  val Jfge          = 0x1c5
  val JisNaN        = 0x1c8
  val JisInf        = 0x1c9
  
  def name(opcodeNum: Int) = opcodeNum match {
    case AccelFunc     => "accelfunc"
    case AccelParam    => "accelparam"
    case Acos          => "acos"
    case Add           => "add"
    case Aload         => "aload"
    case Aloadb        => "aloadb"
    case AloadBit      => "aloadbit"
    case Aloads        => "aloads"
    case Asin          => "asin"
    case Astore        => "astore"
    case Astoreb       => "astoreb"
    case AstoreBit     => "astorebit"
    case Astores       => "astores"
    case Atan          => "atan"
    case Atan2         => "atan2"
    case BinarySearch  => "binarysearch"
    case Bitand        => "bitand"
    case Bitnot        => "bitnot"
    case Bitor         => "bitor"
    case Bitxor        => "bitxor"
    case Call          => "call"
    case Callf         => "callf"
    case Callfi        => "callfi"
    case Callfii       => "callfii"
    case Callfiii      => "callfiii"
    case Catch         => "catch"
    case Ceil          => "ceil"
    case Copy          => "copy"
    case Copyb         => "copyb"
    case Copys         => "copys"
    case Cos           => "cos"
    case DebugTrap     => "debugtrap"
    case Div           => "div"
    case Exp           => "exp"
    case Fadd          => "fadd"
    case Fdiv          => "fdiv"
    case Floor         => "floor"
    case Fmod          => "fmod"
    case Fmul          => "fmul"
    case Fsub          => "fsub"
    case FtoNumZ       => "ftonumz"
    case FtoNumN       => "ftonumn"
    case Gestalt       => "gestalt"
    case GetIOSys      => "getiosys"
    case GetMemSize    => "getmemsize"
    case GetStringTbl  => "getstringtbl"
    case Glk           => "glk"
    case Jeq           => "jeq"
    case Jfeq          => "jfeq"
    case Jfge          => "jfge"
    case Jfgt          => "jfgt"
    case Jfle          => "jfle"
    case Jflt          => "jflt"
    case Jfne          => "jfne"
    case Jge           => "jge"
    case Jgeu          => "jgeu"
    case Jgt           => "jgt"
    case Jgtu          => "jgtu"
    case JisNaN        => "jisnan"
    case JisInf        => "jisinf"
    case Jle           => "jle"
    case Jleu          => "jleu"
    case Jlt           => "jlt"
    case Jltu          => "jltu"
    case Jne           => "jne"
    case Jnz           => "jnz"
    case Jump          => "jump"
    case JumpAbs       => "jumpabs"
    case Jz            => "jz"
    case LinearSearch  => "linearsearch"
    case LinkedSearch  => "linkedsearch"
    case Log           => "log"
    case Malloc        => "malloc"
    case Mcopy         => "mcopy"
    case Mfree         => "mfree"
    case Mul           => "mul"
    case Mzero         => "mzero"
    case Mod           => "mod"
    case Neg           => "neg"
    case Nop           => "nop"
    case NumToF        => "numtof"
    case Pow           => "pow"
    case Protect       => "protect"
    case Quit          => "quit"
    case Random        => "random"
    case Restart       => "restart"
    case Restore       => "restore"
    case RestoreUndo   => "restoreundo"
    case Return        => "return"
    case Save          => "save"
    case SaveUndo      => "saveundo"
    case SetIOSys      => "setiosys"
    case SetMemSize    => "setmemsize"
    case SetRandom     => "setrandom"
    case SetStringTbl  => "setstringtbl"
    case Sexb          => "sexb"
    case Sexs          => "sexs"
    case ShiftL        => "shiftl"
    case Sin           => "sin"
    case Sqrt          => "sqrt"
    case SShiftR       => "sshiftr"
    case StkCopy       => "stkcopy"
    case StkCount      => "stkcount"
    case StkPeek       => "stkpeek"
    case StkRoll       => "stkroll"
    case StkSwap       => "stkswap"
    case StreamChar    => "streamchar"
    case StreamNum     => "streamnum"
    case StreamStr     => "streamstr"
    case StreamUniChar => "streamunichar"
    case Sub           => "sub"
    case TailCall      => "tailcall"
    case Tan           => "tan"
    case Throw         => "throw"
    case UShiftR       => "ushiftr"
    case Verify        => "verify"
    case _ =>
      throw new IllegalArgumentException(
        "unknown opcode %02x: ".format(opcodeNum))
  }

  def numOperands(opcodeNum : Int) = opcodeNum match {
    case AccelFunc     => 2
    case AccelParam    => 2
    case Acos          => 2
    case Add           => 3
    case Aload         => 3
    case Aloadb        => 3
    case AloadBit      => 3
    case Aloads        => 3
    case Asin          => 2
    case Astore        => 3
    case Astoreb       => 3
    case AstoreBit     => 3
    case Astores       => 3
    case Atan          => 2
    case Atan2         => 3
    case BinarySearch  => 8
    case Bitand        => 3
    case Bitnot        => 2
    case Bitor         => 3
    case Bitxor        => 3
    case Call          => 3
    case Callf         => 2
    case Callfi        => 3
    case Callfii       => 4
    case Callfiii      => 5
    case Catch         => 2
    case Ceil          => 2
    case Copy          => 2
    case Copyb         => 2
    case Copys         => 2
    case Cos           => 2
    case DebugTrap     => 1
    case Div           => 3
    case Exp           => 2
    case Fadd          => 3
    case Fdiv          => 3
    case Floor         => 2
    case Fmod          => 4
    case Fmul          => 3
    case Fsub          => 3
    case FtoNumN       => 2
    case FtoNumZ       => 2
    case Gestalt       => 3
    case GetIOSys      => 2
    case GetMemSize    => 1
    case GetStringTbl  => 1
    case Glk           => 3
    case Jeq           => 3
    case Jfeq          => 4
    case Jfge          => 3
    case Jfgt          => 3
    case Jfle          => 3
    case Jflt          => 3
    case Jfne          => 4
    case Jge           => 3
    case Jgeu          => 3
    case Jgt           => 3
    case Jgtu          => 3
    case JisNaN        => 2
    case JisInf        => 2
    case Jle           => 3
    case Jleu          => 3
    case Jlt           => 3
    case Jltu          => 3
    case Jne           => 3
    case Jnz           => 2
    case Jump          => 1
    case JumpAbs       => 1
    case Jz            => 2
    case LinearSearch  => 8
    case LinkedSearch  => 7
    case Log           => 2
    case Malloc        => 2
    case Mcopy         => 3
    case Mfree         => 1
    case Mzero         => 2
    case Mod           => 3
    case Mul           => 3
    case Neg           => 2
    case Nop           => 0
    case NumToF        => 2
    case Pow           => 3
    case Protect       => 2
    case Quit          => 0
    case Random        => 2
    case Restart       => 0
    case Restore       => 2
    case RestoreUndo   => 1
    case Return        => 1
    case Save          => 2
    case SaveUndo      => 1
    case SetIOSys      => 2
    case SetMemSize    => 2
    case SetRandom     => 1
    case SetStringTbl  => 1
    case Sexb          => 2
    case Sexs          => 2
    case ShiftL        => 3
    case Sin           => 2
    case Sqrt          => 2
    case SShiftR       => 3
    case StkCopy       => 1
    case StkCount      => 1
    case StkPeek       => 2
    case StkRoll       => 2
    case StkSwap       => 0
    case StreamChar    => 1
    case StreamNum     => 1
    case StreamStr     => 1
    case StreamUniChar => 1
    case Sub           => 3
    case TailCall      => 2
    case Tan           => 2
    case Throw         => 2
    case UShiftR       => 3
    case Verify        => 1
    case _ =>
      throw new IllegalArgumentException(
        "unknown opcode %02x: ".format(opcodeNum))
  }  
}

class GlulxStoryHeader(mem : Memory) {
  def isGlulx       = mem.intAt(0) == 0x476c756c // 'Glul'
  def version       = mem.intAt(4)
  def ramstart      = mem.intAt(8)
  def extstart      = mem.intAt(12)
  def endmem        = mem.intAt(16)
  def stacksize     = mem.intAt(20)
  def startfunc     = mem.intAt(24)
  def decodingTable = mem.intAt(28)
  def checksum      = mem.intAt(32)
}

class Operand(var addressMode: Int, var value: Int) {
  def this() = this(0, 0)
  
  override def toString = {
    if (addressMode <= 3) "#$%02x".format(value)
    else if (addressMode >= 5 && addressMode <= 7) "MEM$%02x".format(value)
    else if (addressMode == 8) "(SP)"
    else if (addressMode >= 9 && addressMode <= 11) "L%02x".format(value)
    else "RAM$%02x".format(value)
  }
}

class LocalDescriptor {
  var localType  = 0
  var localCount = 0
}

object GlulxGestalt {
  val logger = Logger.getLogger("glulx.gestalt")
  val Version      = 0
  val TerpVersion  = 1
  val ResizeMem    = 2
  val Undo         = 3
  val IOSystem     = 4
  val Unicode      = 5
  val MemCopy      = 6
  val MAlloc       = 7
  val MAllocHeap   = 8
  val Acceleration = 9
  val AccelFunc    = 10
  val GlulxFloat   = 11
  def gestalt(selector: Int, param: Int): Int = {
    logger.info("Glulx.gestalt(#$%02x, #$%02x)".format(selector, param))
    selector match {
      case Version      => 0x00030102
      case TerpVersion  => 0x00010000
      case ResizeMem    => 1
      case Undo         => 1
      case IOSystem     => if (param >= 0 && param <= 2) 1 else 0
      case Unicode      => 1
      case MemCopy      => 1
      case MAlloc       => 1
      case Acceleration => 1
      case AccelFunc    => if (param >= 1 && param <= 7) 1 else 0
      case GlulxFloat   => 1
      case _               => 0
    }
  }
}

/**
 * A class that is only concerned with keeping serializable, persistent
 * VM state. VMState is too much concerned with execution support, so it
 * makes sense, to keep things in a simpler data structure for undo and
 * similar things.
 */
class Snapshot(val ram: Array[Byte], val stack: Array[Byte],
               val extMem: Array[Byte]) {
}
