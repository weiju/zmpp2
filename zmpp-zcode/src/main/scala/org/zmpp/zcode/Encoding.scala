/*
 * Created on 2010/05/13
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
package org.zmpp.zcode

import org.zmpp.base.Memory

/*
 * String decoding/encoding functionality is found here.
 */
trait Alphabet {
  def name: String
  def lookup(zchar: Int): Char
  def charCodeFor(c: Char): Int
  def contains(c: Char): Boolean
}

abstract class AbstractAlphabet extends Alphabet {
  // representation as array is more efficient than string
  def table: Array[Char]
  def lookup(zchar: Int): Char = table(zchar - 6)

  def charCodeFor(c: Char): Int = {
    var i = 0
    while (i < table.length) {
      if (table(i) == c) return i + 6
      i += 1
    }
    throw new IllegalArgumentException("Character '%c' not found".format(c))
  }
  def contains(c: Char) = table.filter{tableChar => c == tableChar}.length > 0
}
object Alphabet0 extends AbstractAlphabet {
  val table = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
                    'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                    'w', 'x', 'y', 'z')
  def name = "A0"
}
object Alphabet1 extends AbstractAlphabet {
  val table = Array('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
                    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                    'W', 'X', 'Y', 'Z')
  def name = "A1"
}
object Alphabet2 extends AbstractAlphabet {
  val table = Array(' ', '\n', '0', '1', '2', '3', '4', '5', '6', '7', '8',
                    '9', '.', ',', '!', '?', '_', '#', '\'', '"', '/', '\\',
                    '-', ':', '(', ')')
  def name = "A2"
}
object Alphabet2_V1 extends AbstractAlphabet {
  val table = Array(' ', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                    '.', ',', '!', '?', '_', '#', '\'', '"', '/', '\\', '<',
                    '-', ':', '(', ')')
  def name = "A2"
}

class CustomAlphabet(val name: String, memory: Memory, address: Int) extends Alphabet {
  def lookup(zchar: Int): Char = memory.byteAt(address + zchar - 6).asInstanceOf[Char]
  def charCodeFor(c: Char): Int = {
    var i = 0
    while (i < 26) {
      if (memory.byteAt(address + i) == c) return i + 6
      i += 1
    }
    throw new IllegalArgumentException("Character '%c' not found".format(c))
  }
  def contains(c: Char): Boolean = {
    var i = 0
    while (i < 26) {
      if (memory.byteAt(address + i) == c) return true
      i += 1
    }
    false
  }
}

trait AccentTable {
  def apply(index: Int): Char
  // returns the index of the specified character or -1 if not found
  def indexOf(c: Char): Int
}

object DefaultAccentTable extends AccentTable {
  val StandardAccents: Array[Char] = Array(
    '\u00e4', '\u00f6', '\u00fc', '\u00c4', '\u00d6', '\u00dc', '\u00df',
    '\u00bb', '\u00ab',
    '\u00eb', '\u00ef', '\u00ff', '\u00cb', '\u00cf',
    '\u00e1', '\u00e9', '\u00ed', '\u00f3', '\u00fa', '\u00fd',
    '\u00c1', '\u00c9', '\u00cd', '\u00d3', '\u00da', '\u00dd',
    '\u00e0', '\u00e8', '\u00ec', '\u00f2', '\u00f9',
    '\u00c0', '\u00c8', '\u00cc', '\u00d2', '\u00d9',
    '\u00e2', '\u00ea', '\u00ee', '\u00f4', '\u00fb',
    '\u00c2', '\u00ca', '\u00ce', '\u00d4', '\u00db',
    '\u00e5', '\u00c5', '\u00f8', '\u00d8',
    '\u00e3', '\u00f1', '\u00f5', '\u00c3', '\u00d1', '\u00d5',
    '\u00e6', '\u00c6', '\u00e7', '\u00c7',
    '\u00fe', '\u00fd', '\u00f0', '\u00d0',
    '\u00a3', '\u0153', '\u0152', '\u00a1', '\u00bf')

  def apply(index: Int) = {
    if (index < StandardAccents.length) StandardAccents(index) else '?'
  }
  def indexOf(c: Char): Int = {
    var i = 0
    while (i < StandardAccents.length) {
      if (StandardAccents(i) == c) return i
      i += 1
    }
    -1
  }
}

class CustomAccentTable(memory: Memory, address: Int) extends AccentTable {
  val numWords = memory.byteAt(address)

  def apply(index: Int) = {
    if (index < numWords)
      memory.shortAt(address + 1 + index * 2).asInstanceOf[Char]
    else '?'
  }
  def indexOf(c: Char): Int = {
    var i = 0
    while (i < numWords) {
      if (memory.shortAt(address + 1 + i * 2) == c) return i
      i += 1
    }
    -1
  }
}

object ZsciiEncoding {
  val NullChar = 0  
  def zsciiCodeFor(c: Int) = c
  val AccentStart = 155
  val AccentEnd   = 251
  def isZSCIIAccent(c: Char) = c >= AccentStart && c <= AccentEnd
}

class ZsciiEncoding(_state: VMState) {

  import ZsciiEncoding._

  private[this] var A0: Alphabet = null
  private[this] var A1: Alphabet = null
  private[this] var A2: Alphabet = null
  private[this] var accentTable: AccentTable = DefaultAccentTable

  // processing state: abbreviations and multi-character sequences
  var currentAlphabet: Alphabet      = A0
  var lastAlphabet: Alphabet         = A0
  var currentAbbreviation            = 0
  var decode10bit                    = false
  var decode10bitStage               = 0
  var decode10bitFirst               = 0
  var shiftLock                      = false

  // called when the state object has changed
  def resetVMState {
    A0 = Alphabet0
    A1 = Alphabet1
    A2 = if (_state.header.version == 1) Alphabet2_V1 else Alphabet2   
    accentTable = DefaultAccentTable

    if (_state.header.version >= 5) {
      if (_state.header.alphabetTable > 0) {
        A0 = new CustomAlphabet("CA0", _state.story, _state.header.alphabetTable)
        A1 = new CustomAlphabet("CA1", _state.story, _state.header.alphabetTable + 26)
        A2 = new CustomAlphabet("CA2", _state.story, _state.header.alphabetTable + 52)
      }
      if (_state.header.customAccentTable > 0) {
        accentTable = new CustomAccentTable(_state.story, _state.header.customAccentTable)
      }
    }
    resetEncodingState
  }

  // called to reset encoding
  def resetEncodingState {
    currentAlphabet = A0
    lastAlphabet    = A0
    decode10bit     = false
    shiftLock       = false
  }

  def isShiftCharacter(zchar: Int) = {
    zchar == 4 || zchar == 5 ||
    (_state.header.version <= 2 && (zchar == 2 || zchar == 3))
  }

  private def handleShift(zchar: Int) {
    if (_state.header.version <= 2) handleShiftV1V2(zchar)
    else handleShiftStd(zchar)
  }
  private def handleShiftV1V2(zchar: Int) = {
    lastAlphabet = currentAlphabet
    zchar match {
      case 2 => doShiftCode2
      case 3 => doShiftCode3
      case 4 => doShiftCode2
      case 5 => doShiftCode3
    }
    shiftLock = (zchar == 4) || (zchar == 5) 
  }
  private def doShiftCode2 {
    if (currentAlphabet == A0) currentAlphabet = A1
    else if (currentAlphabet == A1) currentAlphabet = A2
    else if (currentAlphabet == A2) currentAlphabet = A0
  }
  private def doShiftCode3 {
    if (currentAlphabet == A0) currentAlphabet = A2
    else if (currentAlphabet == A1) currentAlphabet = A0
    else if (currentAlphabet == A2) currentAlphabet = A1
  }
  private def handleShiftStd(zchar: Int) = zchar match {
    case 4 => currentAlphabet = A1
    case 5 => currentAlphabet = A2
  }
  private def unshiftAlphabet = {
    currentAlphabet = if (_state.header.version <= 2) lastAlphabet else A0
  }
  private def isV1Newline(zchar: Int) = _state.header.version == 1 && zchar == 1
  private def isAbbreviationCharacter(zchar: Int) = {
    (_state.header.version >= 3 && zchar >= 1 && zchar <= 3) ||
    (_state.header.version == 2 && zchar == 1)
  }
  def zsciiToUnicode(zsciiChar: Char) = {
    if (isZSCIIAccent(zsciiChar)) accentTable(zsciiChar - AccentStart)
    else zsciiChar
  }
  def unicodeToZSCII(c: Char) = {
    val accentIndex = accentTable.indexOf(c)
    if (accentIndex >= 0) AccentStart + accentIndex
    else c & 0xff
  }

  def decodeZchar(zchar: Int, stream: OutputStream) {
    if (currentAbbreviation != 0) {
      //printf("process abbreviation: %d zchar: %02x ALPHABET = %s\n",
      //       currentAbbreviation, zchar, currentAlphabet.name)
      var entryNum = 32 * (currentAbbreviation - 1) + zchar
      currentAbbreviation = 0 // mark abbreviation as processed
      var abbrevAddr = _state.shortAt(_state.header.abbrevTable + entryNum * 2)

      // save old state
      val oldCurrentAlphabet = currentAlphabet
      val oldLastAlphabet = lastAlphabet
      val oldShiftLock = shiftLock
      shiftLock = false
      decodeZStringAtByteAddress(abbrevAddr * 2, stream)

      // restore old state
      currentAlphabet = oldCurrentAlphabet
      lastAlphabet = oldLastAlphabet
      shiftLock = oldShiftLock
    } else if (decode10bit) {
      if (decode10bitStage == 2) {
        // second half of decode 10 bit
        val char10 = (decode10bitFirst << 5) | zchar 
        decode10bit = false
        //printf("END 10 bit decoding, second: %02x, merged: %02x (%c)\n",
        //       zchar, char10, char10)
        stream.putChar(char10.asInstanceOf[Char])
      } else {
        decode10bitFirst = zchar
        decode10bitStage += 1
        //printf("IN 10 bit decoding, first: %02x\n", zchar)
      }
    }
    else if (zchar == 0) stream.putChar(' ')
    else if (zchar == 7 && currentAlphabet == A2) stream.putChar('\n')
    else if (isV1Newline(zchar)) stream.putChar('\n')
    else if (isShiftCharacter(zchar)) handleShift(zchar)
    else if (isAbbreviationCharacter(zchar)) {
      if (currentAbbreviation == 0) currentAbbreviation = zchar
    } else if (zchar == 6 && currentAlphabet == A2) {
      // 10-bit mode
      //println("START 10 bit MODE")
      decode10bit      = true
      decode10bitStage = 1
    }
    else if (zchar > 5) {
      stream.putChar(currentAlphabet.lookup(zchar))
      //printf("decoded ZCHAR '%c' (zchar=%d, Alphabet='%s')\n",
      //       currentAlphabet.lookup(zchar), zchar, currentAlphabet.name)
    }
    // always reset the alphabet if not shift
    if (!shiftLock && !isShiftCharacter(zchar)) unshiftAlphabet
  }
  def decodeZString(stream: OutputStream) {
    _state.pc += decodeZStringAtByteAddress(_state.pc, stream)
  }
  def decodeZStringAtByteAddress(addr: Int, stream: OutputStream) = {
    var numBytesDecoded = 0
    var currentWord = _state.shortAt(addr)
    var endofText   = false
    resetEncodingState
    while (!endofText) {
      numBytesDecoded += 2
      //printf("CURRENT WORD: %02x (Alphabet: %s)\n", currentWord,
      //       currentAlphabet.name)
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
    // not sure if we should always reset the decoder, but it seems to work
    // what happens in nested invocations (abbreviations ?)
    numBytesDecoded
  }
  def decodeZStringAtPackedAddress(paddr: Int, stream: OutputStream) {
    decodeZStringAtByteAddress(_state.header.unpackStringAddress(paddr), stream)
  }

  // This function returns the shift code:
  // 0: no shift needed - it's A0
  // 4: A1
  // 5: A2
  // -1 not in any alphabet -> 10bit code 
  def shiftCodeFor(c: Char): Int = {
    if (A0.contains(c)) 0
    else if (A1.contains(c)) 4
    else if (A2.contains(c)) 5
    else -1
  }

  def charCodeFor(c: Char): Int = {
    if (A0.contains(c)) A0.charCodeFor(c)
    else if (A1.contains(c)) A1.charCodeFor(c)
    else if (A2.contains(c)) A2.charCodeFor(c)
    else {
      throw new IllegalArgumentException("char not found in alphabet")
    }
  }
}

