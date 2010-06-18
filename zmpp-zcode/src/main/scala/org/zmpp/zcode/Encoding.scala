/*
 * Created on 2010/05/13
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
package org.zmpp.zcode

/*
 * String decoding/encoding functionality is found here.
 */
 
trait Alphabet {
  def lookup(zchar: Int): Char
  def name: String
}
class Alphabet0 extends Alphabet {
  val _table = "abcdefghijklmnopqrstuvwxyz"
  def lookup(zchar: Int): Char = _table(zchar - 6)
  def name = "A0" // debugging
}
class Alphabet1 extends Alphabet {
  val _table = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  def lookup(zchar: Int): Char = _table(zchar - 6)
  def name = "A1" // debugging
}
class Alphabet2 extends Alphabet {
  val _table = " \n0123456789.,!?+#'\"/\\-:()"
  def lookup(zchar: Int): Char = _table(zchar - 6)
  def name = "A2" // just debugging
}

class ZsciiEncoding(_state: VMState) {

  private val A0 = new Alphabet0
  private val A1 = new Alphabet1
  private val A2 = new Alphabet2

  // processing state: abbreviations and multi-character sequences
  private var currentAlphabet: Alphabet = A0
  private var currentAbbreviation       = 0
  private var decode10bit               = false
  private var decode10bitStage          = 0
  private var decode10bitFirst          = 0

  def reset {
    currentAlphabet = A0
    decode10bit     = false
  }

  private def decodeZchar(zchar: Int, stream: OutputStream) {
    if (currentAbbreviation != 0) {
      //printf("process abbreviation: %d zchar: %02x ALPHABET = %s\n",
      //       currentAbbreviation, zchar, currentAlphabet.name)
      var entryNum = 32 * (currentAbbreviation - 1) + zchar
      currentAbbreviation = 0 // mark abbreviation as processed
      var abbrevAddr = _state.shortAt(_state.header.abbrevTable + entryNum * 2)
      decodeZStringAtByteAddress(abbrevAddr * 2, stream)
    } else if (decode10bit) {
      if (decode10bitStage == 2) {
        // second half of decode 10 bit
        val char10 = (decode10bitFirst << 5) | zchar 
        decode10bit = false
        printf("END 10 bit decoding, second: %02x, merged: %02x\n", zchar, char10)
        stream.printChar(char10.asInstanceOf[Char])
      } else {
        decode10bitFirst = zchar
        decode10bitStage += 1
        printf("IN 10 bit decoding, first: %02x\n", zchar)
      }
    }
    else if (zchar == 0) {
      stream.printChar(' ')
    }
    else if (zchar >= 1 && zchar <= 3) {
      if (currentAbbreviation == 0) currentAbbreviation = zchar
    }
    else if (zchar == 4) currentAlphabet = A1
    else if (zchar == 5) currentAlphabet = A2
    else if (zchar == 6 && currentAlphabet == A2) {
      // 10-bit mode
      println("START 10bit MODE")
      decode10bit      = true
      decode10bitStage = 1
    }
    else if (zchar > 5) {
      stream.printChar(currentAlphabet.lookup(zchar))
    }
    // always reset the alphabet if not shift
    if (zchar != 4 && zchar != 5) currentAlphabet = A0
  }
  def decodeZString(stream: OutputStream) {
    _state.pc += decodeZStringAtByteAddress(_state.pc, stream)
  }
  def decodeZStringAtByteAddress(addr: Int, stream: OutputStream) = {
    var numBytesDecoded = 0
    var currentWord = _state.shortAt(addr)
    var endofText   = false

    while (!endofText) {
      numBytesDecoded += 2
      //printf("CURRENT WORD: %02x\n", currentWord)
      val zchar0 = (currentWord >> 10) & 0x1f
      val zchar1 = (currentWord >>  5) & 0x1f
      val zchar2 = currentWord & 0x1f
      //printf("c0 = %02x c1 = %02x c2 = %02x\n", zchar0, zchar1, zchar2)
      decodeZchar(zchar0, stream)
      decodeZchar(zchar1, stream)
      decodeZchar(zchar2, stream)
      endofText = (currentWord & 0x8000) == 0x8000
      if (!endofText) {
        currentWord = _state.shortAt(addr + numBytesDecoded)
      }
    }
    numBytesDecoded
  }
  def decodeZStringAtPackedAddress(paddr: Int, stream: OutputStream) {
    decodeZStringAtByteAddress(_state.header.unpackStringAddress(paddr), stream)
  }
}
