/*
 * Created on 2011/02/12
 * Copyright (c) 2010-2012, Wei-ju Wu.
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
package org.zmpp.glulx;

/**
 * This Opcodes class is only kept around for testing. It used to be part of the
 * main implementation, but alas, it actually not that useful - and it bloats
 * the core anyways.
 */
public class Opcodes {
    public static final int Nop           = 0x00;
    public static final int Add           = 0x10;
    public static final int Sub           = 0x11;
    public static final int Mul           = 0x12;
    public static final int Div           = 0x13;
    public static final int Mod           = 0x14;
    public static final int Neg           = 0x15;
    public static final int Bitand        = 0x18;
    public static final int Bitor         = 0x19;
    public static final int Bitxor        = 0x1a;
    public static final int Bitnot        = 0x1b;
    public static final int ShiftL        = 0x1c;
    public static final int SShiftR       = 0x1d;
    public static final int UShiftR       = 0x1e;
    public static final int Jump          = 0x20;
    public static final int Jz            = 0x22;
    public static final int Jnz           = 0x23;
    public static final int Jeq           = 0x24;
    public static final int Jne           = 0x25;
    public static final int Jlt           = 0x26;
    public static final int Jge           = 0x27;
    public static final int Jgt           = 0x28;
    public static final int Jle           = 0x29;
    public static final int Jltu          = 0x2a;
    public static final int Jgeu          = 0x2b;
    public static final int Jgtu          = 0x2c;
    public static final int Jleu          = 0x2d;
    public static final int Call          = 0x30;
    public static final int Return        = 0x31;
    public static final int Catch         = 0x32;
    public static final int Throw         = 0x33;
    public static final int TailCall      = 0x34;
    public static final int Copy          = 0x40;
    public static final int Copys         = 0x41;
    public static final int Copyb         = 0x42;
    public static final int Sexs          = 0x44;
    public static final int Sexb          = 0x45;
    public static final int Aload         = 0x48;
    public static final int Aloads        = 0x49;
    public static final int Aloadb        = 0x4a;
    public static final int AloadBit      = 0x4b;
    public static final int Astore        = 0x4c;
    public static final int Astores       = 0x4d;
    public static final int Astoreb       = 0x4e;
    public static final int AstoreBit     = 0x4f;
    public static final int StkCount      = 0x50;
    public static final int StkPeek       = 0x51;
    public static final int StkSwap       = 0x52;
    public static final int StkRoll       = 0x53;
    public static final int StkCopy       = 0x54;
    public static final int StreamChar    = 0x70;
    public static final int StreamNum     = 0x71;
    public static final int StreamStr     = 0x72;
    public static final int StreamUniChar = 0x73;
    public static final int Gestalt       = 0x100;
    public static final int DebugTrap     = 0x101;
    public static final int GetMemSize    = 0x102;
    public static final int SetMemSize    = 0x103;
    public static final int JumpAbs       = 0x104;
    public static final int Random        = 0x110;
    public static final int SetRandom     = 0x111;
    public static final int Quit          = 0x120;
    public static final int Verify        = 0x121;
    public static final int Restart       = 0x122;
    public static final int Save          = 0x123;
    public static final int Restore       = 0x124;
    public static final int SaveUndo      = 0x125;
    public static final int RestoreUndo   = 0x126;
    public static final int Protect       = 0x127;
    public static final int Glk           = 0x130;
    public static final int GetStringTbl  = 0x140;
    public static final int SetStringTbl  = 0x141;
    public static final int GetIOSys      = 0x148;
    public static final int SetIOSys      = 0x149;
    public static final int LinearSearch  = 0x150;
    public static final int BinarySearch  = 0x151;
    public static final int LinkedSearch  = 0x152;
    public static final int Callf         = 0x160;
    public static final int Callfi        = 0x161;
    public static final int Callfii       = 0x162;
    public static final int Callfiii      = 0x163;
    public static final int Mzero         = 0x170;
    public static final int Mcopy         = 0x171;
    public static final int Malloc        = 0x178;
    public static final int Mfree         = 0x179;
    public static final int AccelFunc     = 0x180;
    public static final int AccelParam    = 0x181;

    public static final int NumToF        = 0x190;
    public static final int FtoNumZ       = 0x191;
    public static final int FtoNumN       = 0x192;
    public static final int Ceil          = 0x198;
    public static final int Floor         = 0x199;
    public static final int Fadd          = 0x1a0;
    public static final int Fsub          = 0x1a1;
    public static final int Fmul          = 0x1a2;
    public static final int Fdiv          = 0x1a3;
    public static final int Fmod          = 0x1a4;
    public static final int Sqrt          = 0x1a8;
    public static final int Exp           = 0x1a9;
    public static final int Log           = 0x1aa;
    public static final int Pow           = 0x1ab;
    public static final int Sin           = 0x1b0;
    public static final int Cos           = 0x1b1;
    public static final int Tan           = 0x1b2;
    public static final int Asin          = 0x1b3;
    public static final int Acos          = 0x1b4;
    public static final int Atan          = 0x1b5;
    public static final int Atan2         = 0x1b6;
    public static final int Jfeq          = 0x1c0;
    public static final int Jfne          = 0x1c1;
    public static final int Jflt          = 0x1c2;
    public static final int Jfle          = 0x1c3;
    public static final int Jfgt          = 0x1c4;
    public static final int Jfge          = 0x1c5;
    public static final int JisNaN        = 0x1c8;
    public static final int JisInf        = 0x1c9;
    public static final int FyreCall      = 0x1000;
      /*  
  def name(opcodeNum: Int) = {
    opcodeNum match {
    case AccelFunc     : "accelfunc"
    case AccelParam    : "accelparam"
    case Acos          : "acos"
    case Add           : "add"
    case Aload         : "aload"
    case Aloadb        : "aloadb"
    case AloadBit      : "aloadbit"
    case Aloads        : "aloads"
    case Asin          : "asin"
    case Astore        : "astore"
    case Astoreb       : "astoreb"
    case AstoreBit     : "astorebit"
    case Astores       : "astores"
    case Atan          : "atan"
    case Atan2         : "atan2"
    case BinarySearch  : "binarysearch"
    case Bitand        : "bitand"
    case Bitnot        : "bitnot"
    case Bitor         : "bitor"
    case Bitxor        : "bitxor"
    case Call          : "call"
    case Callf         : "callf"
    case Callfi        : "callfi"
    case Callfii       : "callfii"
    case Callfiii      : "callfiii"
    case Catch         : "catch"
    case Ceil          : "ceil"
    case Copy          : "copy"
    case Copyb         : "copyb"
    case Copys         : "copys"
    case Cos           : "cos"
    case DebugTrap     : "debugtrap"
    case Div           : "div"
    case Exp           : "exp"
    case Fadd          : "fadd"
    case Fdiv          : "fdiv"
    case Floor         : "floor"
    case Fmod          : "fmod"
    case Fmul          : "fmul"
    case Fsub          : "fsub"
    case FtoNumZ       : "ftonumz"
    case FtoNumN       : "ftonumn"
    case FyreCall      : "fyrecall"
    case Gestalt       : "gestalt"
    case GetIOSys      : "getiosys"
    case GetMemSize    : "getmemsize"
    case GetStringTbl  : "getstringtbl"
    case Glk           : "glk"
    case Jeq           : "jeq"
    case Jfeq          : "jfeq"
    case Jfge          : "jfge"
    case Jfgt          : "jfgt"
    case Jfle          : "jfle"
    case Jflt          : "jflt"
    case Jfne          : "jfne"
    case Jge           : "jge"
    case Jgeu          : "jgeu"
    case Jgt           : "jgt"
    case Jgtu          : "jgtu"
    case JisNaN        : "jisnan"
    case JisInf        : "jisinf"
    case Jle           : "jle"
    case Jleu          : "jleu"
    case Jlt           : "jlt"
    case Jltu          : "jltu"
    case Jne           : "jne"
    case Jnz           : "jnz"
    case Jump          : "jump"
    case JumpAbs       : "jumpabs"
    case Jz            : "jz"
    case LinearSearch  : "linearsearch"
    case LinkedSearch  : "linkedsearch"
    case Log           : "log"
    case Malloc        : "malloc"
    case Mcopy         : "mcopy"
    case Mfree         : "mfree"
    case Mul           : "mul"
    case Mzero         : "mzero"
    case Mod           : "mod"
    case Neg           : "neg"
    case Nop           : "nop"
    case NumToF        : "numtof"
    case Pow           : "pow"
    case Protect       : "protect"
    case Quit          : "quit"
    case Random        : "random"
    case Restart       : "restart"
    case Restore       : "restore"
    case RestoreUndo   : "restoreundo"
    case Return        : "return"
    case Save          : "save"
    case SaveUndo      : "saveundo"
    case SetIOSys      : "setiosys"
    case SetMemSize    : "setmemsize"
    case SetRandom     : "setrandom"
    case SetStringTbl  : "setstringtbl"
    case Sexb          : "sexb"
    case Sexs          : "sexs"
    case ShiftL        : "shiftl"
    case Sin           : "sin"
    case Sqrt          : "sqrt"
    case SShiftR       : "sshiftr"
    case StkCopy       : "stkcopy"
    case StkCount      : "stkcount"
    case StkPeek       : "stkpeek"
    case StkRoll       : "stkroll"
    case StkSwap       : "stkswap"
    case StreamChar    : "streamchar"
    case StreamNum     : "streamnum"
    case StreamStr     : "streamstr"
    case StreamUniChar : "streamunichar"
    case Sub           : "sub"
    case TailCall      : "tailcall"
    case Tan           : "tan"
    case Throw         : "throw"
    case UShiftR       : "ushiftr"
    case Verify        : "verify"
    case _ :
      throw new IllegalArgumentException(
        "unknown opcode %02x: ".format(opcodeNum))
    }
  }
      */
    // This function is not called in the VM. Rather, it serves as a means
    // for verification
    public static final int numOperands(int opcodeNum) {
        switch (opcodeNum) {
        case 0x00  : return 0; // nop

        case 0x10  : return 3; // add
        case 0x11  : return 3; // sub
        case 0x12  : return 3; // mul
        case 0x13  : return 3; // div
        case 0x14  : return 3; // mod
        case 0x15  : return 2; // neg
        case 0x18  : return 3; // bitand
        case 0x19  : return 3; // bitor
        case 0x1a  : return 3; // bitxor
        case 0x1b  : return 2; // bitnot
        case 0x1c  : return 3; // shiftl
        case 0x1d  : return 3; // sshiftr
        case 0x1e  : return 3; // ushiftr

        case 0x20  : return 1; // jump
        case 0x22  : return 2; // jz
        case 0x23  : return 2; // jnz
        case 0x24  : return 3; // jeq
        case 0x25  : return 3; // jne
        case 0x26  : return 3; // jlt
        case 0x27  : return 3; // jge
        case 0x28  : return 3; // jgt
        case 0x29  : return 3; // jle
        case 0x2a  : return 3; // jltu
        case 0x2b  : return 3; // jgeu
        case 0x2c  : return 3; // jgtu
        case 0x2d  : return 3; // jleu

        case 0x30  : return 3; // call
        case 0x31  : return 1; // return
        case 0x32  : return 2; // catch
        case 0x33  : return 2; // throw
        case 0x34  : return 2; // tailcall

        case 0x40  : return 2; // copy
        case 0x41  : return 2; // copys
        case 0x42  : return 2; // copyb
        case 0x44  : return 2; // sexs
        case 0x45  : return 2; // sexb
        case 0x48  : return 3; // aload
        case 0x49  : return 3; // aloads
        case 0x4a  : return 3; // aloadb
        case 0x4b  : return 3; // aloadbit
        case 0x4c  : return 3; // astore
        case 0x4d  : return 3; // astores
        case 0x4e  : return 3; // astoreb
        case 0x4f  : return 3; // astorebit

        case 0x50  : return 1; // stkcount
        case 0x51  : return 2; // stkpeek
        case 0x52  : return 0; // stkswap
        case 0x53  : return 2; // stkroll
        case 0x54  : return 1; // stkcopy

        case 0x70  : return 1; // streamchar
        case 0x71  : return 1; // streamnum
        case 0x72  : return 1; // streamstr
        case 0x73  : return 1; // streamunichar

        case 0x100 : return 3; // gestalt
        case 0x101 : return 1; // debugtrap
        case 0x102 : return 1; // getmemsize
        case 0x103 : return 2; // setmemsize
        case 0x104 : return 1; // jumpabs

        case 0x110 : return 2; // random
        case 0x111 : return 1; // setrandom

        case 0x120 : return 0; // quit
        case 0x121 : return 1; // verify
        case 0x122 : return 0; // restart
        case 0x123 : return 2; // save
        case 0x124 : return 2; // restore
        case 0x125 : return 1; // saveundo
        case 0x126 : return 1; // restoreundo
        case 0x127 : return 2; // protect

        case 0x130 : return 3; // glk

        case 0x140 : return 1; // getstringtbl
        case 0x141 : return 1; // setstringtbl
        case 0x148 : return 2; // getiosys
        case 0x149 : return 2; // setiosys

        case 0x150 : return 8; // linearsearch
        case 0x151 : return 8; // binarysearch
        case 0x152 : return 7; // linkedsearch

        case 0x160 : return 2; // callf
        case 0x161 : return 3; // callfi
        case 0x162 : return 4; // callfii
        case 0x163 : return 5; // callfiii

        case 0x170 : return 2; // mzero
        case 0x171 : return 3; // mcopy
        case 0x178 : return 2; // malloc
        case 0x179 : return 1; // mfree

        case 0x180 : return 2; // accelfunc
        case 0x181 : return 2; // accelparam

        case 0x190 : return 2; // numtof
        case 0x191 : return 2; // ftonumz
        case 0x192 : return 2; // ftonumn
        case 0x198 : return 2; // ceil
        case 0x199 : return 2; // floor

        case 0x1a0 : return 3; // fadd
        case 0x1a1 : return 3; // fsub
        case 0x1a2 : return 3; // fmul
        case 0x1a3 : return 3; // fdiv
        case 0x1a4 : return 4; // fmod
        case 0x1a8 : return 2; // sqrt
        case 0x1a9 : return 2; // exp
        case 0x1aa : return 2; // log
        case 0x1ab : return 3; // pow

        case 0x1b0 : return 2; // sin
        case 0x1b1 : return 2; // cos
        case 0x1b2 : return 2; // tan
        case 0x1b3 : return 2; // asin
        case 0x1b4 : return 2; // acos
        case 0x1b5 : return 2; // atan
        case 0x1b6 : return 3; // atan2

        case 0x1c0 : return 4; // jfeq
        case 0x1c1 : return 4; // jfne
        case 0x1c2 : return 3; // jflt
        case 0x1c3 : return 3; // jfle
        case 0x1c4 : return 3; // jfgt
        case 0x1c5 : return 3; // jfge
        case 0x1c8 : return 2; // jisnan
        case 0x1c9 : return 2; // jisinf
        default: return 0;
    }
  }
}
