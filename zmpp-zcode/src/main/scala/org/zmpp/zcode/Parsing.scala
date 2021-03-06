/*
 * Created on 2010/07/1
 * Copyright (c) 2010-2014, Wei-ju Wu.
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

import org.zmpp.base.Types
import org.zmpp.base.Memory
import org.zmpp.base.DefaultMemory

abstract class Dictionary(state: VMState, dictionaryAddress: Int) {
  def numSeparators = state.byteAt(dictionaryAddress)
  def headerSize = numSeparators + 4
  def entryLength = state.byteAt(dictionaryAddress + numSeparators + 1)
  def entryAddressAt(entryNum: Int) = {
    dictionaryAddress + headerSize + entryNum * entryLength
  }
  def numEntries =
    Types.signExtend16(state.shortAt(dictionaryAddress + numSeparators + 2))

  def tokenMatch(tokenBytes: Array[Byte], entryAddress: Int): Int = {
    var i = 0
    while (i < tokenBytes.length) {
      val tokenByte = tokenBytes(i) & 0xff
      val c = state.byteAt(entryAddress + i)
      if (tokenByte != c) return tokenByte - c
      i += 1
    }
    0
  }

  def lookup(tokenBytes: Array[Byte]): Int
}

class UserDictionary(state: VMState, dictionaryAddress: Int)
extends Dictionary(state, dictionaryAddress) {
  def lookup(tokenBytes: Array[Byte]): Int = {
    val n = math.abs(numEntries)
    var i = 0
    // only linear search supported
    while (i < n) {
      val entryAddress = entryAddressAt(i)
      if (tokenMatch(tokenBytes, entryAddress) == 0) return entryAddress
      i += 1
    }
    0
  }
}

class DefaultDictionary(state: VMState)
extends Dictionary(state, state.header.dictionary) {

  def lookup(tokenBytes: Array[Byte]) = {
    lookupBinary(tokenBytes, 0,  numEntries - 1)
  }

  private def lookupBinary(tokenBytes: Array[Byte], left: Int,
                           right: Int): Int = {
    if (left > right) 0
    else {
      val middle = left + (right - left) / 2
      val entryAddress = entryAddressAt(middle)
      val compval = tokenMatch(tokenBytes, entryAddress)
      if (compval < 0) lookupBinary(tokenBytes, left, middle - 1)
      else if (compval > 0) lookupBinary(tokenBytes, middle + 1, right)
      else entryAddress
    }
  }
}

class Token(val start: Int, val end: Int) {
  def length = end - start + 1
  override def toString = {
    "TOKEN[%d-%d] (length = %d)".format(start, end, length)
  }
}

class Encoder(state: VMState) {

  private[this] val numEntryBytes    = if (state.header.version <= 3) 4 else 6
  private[this] val maxEntryChars    = if (state.header.version <= 3) 6 else 9

  val tokenBytes       = new Array[Byte](numEntryBytes)
  private[this] val tokenBuffer      = new DefaultMemory(tokenBytes)

  private[this] var inputOffset  = 0
  private[this] var writePos     = 0
  private[this] var token: Token = null
  
  // value of the current 3-character word that the encoder
  private[this] var currentWord  = 0
  // the current slot in currentWord
  private[this] var currentSlot  = 0

  private def reset(token: Token) {
    inputOffset = 0
    writePos    = 0
    currentWord = 0
    currentSlot = 0
    this.token  = token
  }

  private def nextChar = {
    val c = state.byteAt(token.start + inputOffset)
    inputOffset += 1
    c.asInstanceOf[Char]
  }

  private def hasMoreInput = {
    inputOffset < token.length && inputOffset < maxEntryChars
  }

  private def appendChar(charCode: Int) {
    val shiftWidth = (2 - currentSlot) * 5
    currentWord |= (charCode & 0x1f) << shiftWidth
    currentSlot += 1

    if (currentWordDone && !atLastWord) {
      tokenBuffer.setShortAt(writePos, currentWord & 0xffff)
      writePos    += 2
      currentWord = 0
      currentSlot = 0
    }
  }

  private def currentWordDone = currentSlot > 2
  private def atLastWord = writePos >= numEntryBytes

  def encode(token: Token) = {
    reset(token)
    // 1. process all characters of the token
    while (hasMoreInput) encodeChar
    // 2. pad the remainder with 5's
    val numPadSlots = numFreeSlots
    for (i <- 0 until numPadSlots) appendChar(5)

    // 3. marks the last word by setting the MSB
    // Note: This is actually the last word in the buffer,
    // would we need to actually mark the last 16 bit word
    // in a very short word ?
    val lastWord = tokenBuffer.shortAt(numEntryBytes - 2)
    tokenBuffer.setShortAt(numEntryBytes - 2, lastWord | 0x8000)
    tokenBytes
  }

  private def encodeChar {
    // can be a shifted character or non-shifted
    val zsciiChar = nextChar
    val shiftCode = state.encoding.shiftCodeFor(zsciiChar)
    if (shiftCode == -1) {
      // 10-bit character
      if (numFreeSlots >= 4) {
        appendChar(5)                        // shift to A2
        appendChar(6)                        // escape to 10 bit
        appendChar((zsciiChar >>> 5) & 0x1f) // hi 5 bit
        appendChar(zsciiChar & 0x1f)         // low 5 bit
      } else {
        // pad remaining slots with 5's
        val numPadSlots = numFreeSlots
        for (i <- 0 until numPadSlots) appendChar(5)
      }
    } else {
      if (shiftCode > 0) {
        appendChar(shiftCode)
      }
      appendChar(state.encoding.charCodeFor(zsciiChar))
    }
  }

  private def numFreeSlots = {
    ((numEntryBytes - writePos) / 2) * 3 + (3 - currentSlot)
  }
}

/**
 * This class processes the input that is read from an input stream.
 */
class ParserHelper(state: VMState, textBuffer: Int, parseBuffer: Int,
                   userDictionary: Int, flag: Boolean) {
  private val storyVersion     = state.header.version
  private val textBufferOffset = if (storyVersion < 5) 1 else 2

  private val dictionary = if (userDictionary == 0) {
    new DefaultDictionary(state)
  } else {
    new UserDictionary(state, userDictionary)
  }
  private[this] val encoder = new Encoder(state)

  private def storeInputToTextBuffer(input: String) {
    val inputString = input.toLowerCase
    val offset = textBufferOffset
    val lengthWithoutTerminator = inputString.length - 1
    val terminateChar = inputString.charAt(inputString.length - 1)

    // convert to ZSCII and store in text buffer on the fly
    for (i <- 0 until lengthWithoutTerminator) {
      state.setByteAt(textBuffer + offset + i,
                      state.encoding.unicodeToZSCII(inputString.charAt(i)))
    }
    // write termination information
    if (storyVersion >= 5) {
      val numCharsTyped =
        if (terminateChar == ZsciiEncoding.NullChar) 0
        else inputString.length - 1
      state.setByteAt(textBuffer + 1, numCharsTyped)
    } else {
      // Terminate with 0 byte in versions < 5
      var terminatePos = lengthWithoutTerminator
      if (terminateChar == ZsciiEncoding.NullChar) terminatePos = 0 // cancelled
      state.setByteAt(textBuffer + offset + terminatePos,
                      ZsciiEncoding.NullChar)
    }
  }
  
  private def isSpace(c: Int) = c == ' '.asInstanceOf[Int]
  private def getWordSeparators: List[Int] = {
    var result: List[Int] = Nil
    val dictionaryAddr = state.header.dictionary
    val numSeparators = state.byteAt(dictionaryAddr)
    for (i <- 0 until numSeparators) {
      result ::= ZsciiEncoding.zsciiCodeFor(state.byteAt(dictionaryAddr+i+1))
    }
    result
  }

  private def bufferlen = state.byteAt(textBuffer)
  private def bufferstart = textBuffer + textBufferOffset
  private def charsTypedV5 = state.byteAt(textBuffer + 1)
  private def atInputEnd(currentChar: Int, bufferIndex: Int) = {
    currentChar == ZsciiEncoding.NullChar || bufferIndex >= bufferlen ||
      storyVersion >= 5 && bufferIndex >= charsTypedV5
  }

  // tokenizes the textbuffer and stores the result in the parse buffer
  def tokenize {
    // 1. lexical analysis
    val maxTokens = state.byteAt(parseBuffer)
    val separators = getWordSeparators
    var tokens: List[Token] = Nil
    var token = processNextToken(0, separators)
    while  (token != null && tokens.length <= maxTokens) {
      tokens ::= token
      // the next buffer start index is right after the end of the
      // previous one
      token = processNextToken(token.end - bufferstart + 1, separators)
    }
    // now tokens contains the tokenized list in the correct order
    tokens = tokens.reverse
    // 2. convert each token to Z-encoded string and lookup in dictionary
    //tokens.map{t => printf("Token: %s\n", t.toString)}
    val numTokens = math.min(tokens.length, maxTokens)
    for (i <- 0 until numTokens) lookup(tokens(i), i)

    // write number of tokens in Byte 1
    state.setByteAt(parseBuffer + 1, numTokens)
  }

  private def processNextToken(currentIndex: Int,
                               separators: List[Int]): Token = {
    // index is the current position within the text input buffer
    var index = currentIndex
    var zsciiChar =
      ZsciiEncoding.zsciiCodeFor(state.byteAt(bufferstart + index))
    
    // return null if text end
    if (atInputEnd(zsciiChar, index)) return null
    
    // skip whitespace
    while (isSpace(zsciiChar) && !atInputEnd(zsciiChar, index)) {
      index += 1
      zsciiChar =
        ZsciiEncoding.zsciiCodeFor(state.byteAt(bufferstart + index))
    }
    // return null if text end
    if (atInputEnd(zsciiChar, index)) return null

    val tokenStart = index
    //printf("TOKEN START AT: %d\n", tokenStart)
    // return separator token if match
    if (separators.exists(sep => sep == zsciiChar))
      return new Token(bufferstart + tokenStart, bufferstart + tokenStart)
    
    // regular token
    var separatorSeen = false
    while (!separatorSeen && !atInputEnd(zsciiChar, index)) {
      index += 1
      zsciiChar =
        ZsciiEncoding.zsciiCodeFor(state.byteAt(bufferstart + index))
      if (isSpace(zsciiChar) || separators.exists(sep => sep == zsciiChar)) {
        separatorSeen = true
      }
    }
    new Token(bufferstart + tokenStart, bufferstart + index - 1)
  }

  def process(input: String) {
    val version = storyVersion
    storeInputToTextBuffer(input)
    if (version < 5 || version >= 5 && parseBuffer > 0) {
      tokenize
    }
  }
  
  def lookup(token: Token, tokenNum: Int) = {
    encoder.encode(token)
    // tokenBytes now contains the dictionary-encoded token
    val lookupAddress = dictionary.lookup(encoder.tokenBytes)
    if (!flag || flag && lookupAddress > 0) {
      val address = parseBuffer + 2 + tokenNum * 4
      //printf("($%02x) Token found at: %02x, %d, %02x\n", address, lookupAddress, token.length, token.start)
      state.setShortAt(address, lookupAddress)
      state.setByteAt(address + 2, token.length)
      state.setByteAt(address + 3, token.start - textBuffer)
    }
  }
}
