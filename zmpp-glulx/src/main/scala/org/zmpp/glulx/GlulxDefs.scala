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

// *************************************************************************
// ***** Glulx Definitions
// ***** Rather than defining enumerations for everything, we define
// ***** frequently used constants as simple int values
// *************************************************************************

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
  val FyreCall      = 0x1000
  
  def name(opcodeNum: Int) = {
    opcodeNum match {
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
    case FyreCall      => "fyrecall"
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
  }

  def numOperands(opcodeNum : Int) = {
    (opcodeNum: @switch) match {
      case 0x00  => 0 // nop
      case 0x10  => 3 // add
      case 0x11  => 3 // sub
      case 0x12  => 3 // mul
      case 0x13  => 3 // div
      case 0x14  => 3 // mod
      case 0x15  => 2 // neg
      case 0x18  => 3 // bitand
      case 0x19  => 3 // bitor
      case 0x1a  => 3 // bitxor
      case 0x1b  => 2 // bitnot
      case 0x1c  => 3 // shiftl
      case 0x1d  => 3 // sshiftr
      case 0x1e  => 3 // ushiftr
      case 0x20  => 1 // jump
      case 0x22  => 2 // jz
      case 0x23  => 2 // jnz
      case 0x24  => 3 // jeq
      case 0x25  => 3 // jne
      case 0x26  => 3 // jlt
      case 0x27  => 3 // jge
      case 0x28  => 3 // jgt
      case 0x29  => 3 // jle
      case 0x2a  => 3 // jltu
      case 0x2b  => 3 // jgeu
      case 0x2c  => 3 // jgtu
      case 0x2d  => 3 // jleu
      case 0x30  => 3 // call
      case 0x31  => 1 // return
      case 0x32  => 2 // catch
      case 0x33  => 2 // throw
      case 0x34  => 2 // tailcall
      case 0x40  => 2 // copy
      case 0x41  => 2 // copys
      case 0x42  => 2 // copyb
      case 0x44  => 2 // sexs
      case 0x45  => 2 // sexb
      case 0x48  => 3 // aload
      case 0x49  => 3 // aloads
      case 0x4a  => 3 // aloadb
      case 0x4b  => 3 // aloadbit
      case 0x4c  => 3 // astore
      case 0x4d  => 3 // astores
      case 0x4e  => 3 // astoreb
      case 0x4f  => 3 // astorebit
      case 0x50  => 1 // stkcount
      case 0x51  => 2 // stkpeek
      case 0x52  => 0 // stkswap
      case 0x53  => 2 // stkroll
      case 0x54  => 1 // stkcopy
      case 0x70  => 1 // streamchar
      case 0x71  => 1 // streamnum
      case 0x72  => 1 // streamstr
      case 0x73  => 1 // streamunichar
      case 0x100 => 3 // gestalt
      case 0x101 => 1 // debugtrap
      case 0x102 => 1 // getmemsize
      case 0x103 => 2 // setmemsize
      case 0x104 => 1 // jumpabs
      case 0x110 => 2 // random
      case 0x111 => 1 // setrandom
      case 0x120 => 0 // quit
      case 0x121 => 1 // verify
      case 0x122 => 0 // restart
      case 0x123 => 2 // save
      case 0x124 => 2 // restore
      case 0x125 => 1 // saveundo
      case 0x126 => 1 // restoreundo
      case 0x127 => 2 // protect
      case 0x130 => 3 // glk
      case 0x140 => 1 // getstringtbl
      case 0x141 => 1 // setstringtbl
      case 0x148 => 2 // getiosys
      case 0x149 => 2 // setiosys
      case 0x150 => 8 // linearsearch
      case 0x151 => 8 // binarysearch
      case 0x152 => 7 // linkedsearch
      case 0x160 => 2 // callf
      case 0x161 => 3 // callfi
      case 0x162 => 4 // callfii
      case 0x163 => 5 // callfiii
      case 0x170 => 2 // mzero
      case 0x171 => 3 // mcopy
      case 0x178 => 2 // malloc
      case 0x179 => 1 // mfree
      case 0x180 => 2 // accelfunc
      case 0x181 => 2 // accelparam
      case 0x190 => 2 // numtof
      case 0x191 => 2 // ftonumz
      case 0x192 => 2 // ftonumn
      case 0x198 => 2 // ceil
      case 0x199 => 2 // floor
      case 0x1a0 => 3 // fadd
      case 0x1a1 => 3 // fsub
      case 0x1a2 => 3 // fmul
      case 0x1a3 => 3 // fdiv
      case 0x1a4 => 4 // fmod
      case 0x1a8 => 2 // sqrt
      case 0x1a9 => 2 // exp
      case 0x1aa => 2 // log
      case 0x1ab => 3 // pow
      case 0x1b0 => 2 // sin
      case 0x1b1 => 2 // cos
      case 0x1b2 => 2 // tan
      case 0x1b3 => 2 // asin
      case 0x1b4 => 2 // acos
      case 0x1b5 => 2 // atan
      case 0x1b6 => 3 // atan2
      case 0x1c0 => 4 // jfeq
      case 0x1c1 => 4 // jfne
      case 0x1c2 => 3 // jflt
      case 0x1c3 => 3 // jfle
      case 0x1c4 => 3 // jfgt
      case 0x1c5 => 3 // jfge
      case 0x1c8 => 2 // jisnan
      case 0x1c9 => 2 // jisinf
      case _ =>
        throw new IllegalArgumentException(
          "unknown opcode %02x: ".format(opcodeNum))
    }
  }
}
