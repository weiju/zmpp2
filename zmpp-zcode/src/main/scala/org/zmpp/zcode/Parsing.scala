/*
 * Created on 2010/07/1
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

class DefaultDictionary(state: VMState) {
  
}

class Token(val start: Int, val end: Int) {
  override def toString = {
    "TOKEN[%d-%d]".format(start, end)
  }
}

/**
 * This class processes the input that is read from an input stream.
 */
class ParserHelper(state: VMState, textBuffer: Int, parseBuffer: Int,
                   userDictionary: Int, flag: Boolean) {

  private def storyVersion = state.header.version
  private def textBufferOffset = if (storyVersion < 5) 1 else 2

  private def storeInputToTextBuffer(inputString: String) {
    val offset = textBufferOffset
    for (i <- 0 until inputString.length) {
      state.setByteAt(textBuffer + offset + i,
                      inputString.charAt(i) & 0xff)
    }
    // write termination information (TODO: Check end position)
    val terminateChar = inputString.charAt(inputString.length - 1)
    if (storyVersion >= 5) {
      val numCharsTyped =
        if (terminateChar == ZsciiEncoding.NullChar) 0
        else inputString.length - 1
    } else {
      // Terminate with 0 byte in versions < 5
      var terminatePos = inputString.length
      if (terminateChar == ZsciiEncoding.NullChar) terminatePos = 0 // cancelled
      state.setByteAt(textBuffer + offset + terminatePos, ZsciiEncoding.NullChar)
    }
  }
  
  private def isSpace(c: Int) = c == ' '.asInstanceOf[Int]
  private def getWordSeparators: List[Int] = {
    var result: List[Int] = Nil
    val dictionaryAddr = state.header.dictionary
    val numSeparators = state.byteAt(dictionaryAddr)
    for (i <- 0 until numSeparators) {
      result ::= ZsciiEncoding.zsciiCodeFor(state.byteAt(dictionaryAddr + i + 1))
    }
    result
  }

  // tokenizes the textbuffer and stores the result in the parse buffer
  def tokenize {
    val bufferlen = state.byteAt(textBuffer)
    val bufferstart = textBuffer + textBufferOffset
    val charsTyped = if (storyVersion >= 5) state.byteAt(textBuffer + 1) else 0    
    
    // 1. lexical analysis
    val maxTokens = state.byteAt(parseBuffer)
    val separators = getWordSeparators
    var tokens: List[Token] = Nil
    var token = processNextToken(bufferstart, bufferlen, separators)
    while  (token != null && tokens.length <= maxTokens) {
      tokens ::= token
      token = processNextToken(token.end + 1, bufferlen, separators)
    }
    // now tokens contains the tokenized list in the correct order
    tokens = tokens.reverse
    // 2. convert each token to Z-encoded string and lookup in dictionary
  }

  private def processNextToken(bufferpos: Int, bufferlen: Int,
                               separators: List[Int]): Token = {
    val bufferend = bufferpos + bufferlen
    var pos = bufferpos
    var zsciiChar = ZsciiEncoding.zsciiCodeFor(state.byteAt(pos))
    
    if (zsciiChar == ZsciiEncoding.NullChar) return null // return null if text end
    
    // skip whitespace
    while (isSpace(zsciiChar) && pos < bufferend) {
      pos += 1
      zsciiChar = ZsciiEncoding.zsciiCodeFor(state.byteAt(pos))
    }
    val tokenStart = pos
    // return separator token if match
    if (separators.exists(sep => sep == zsciiChar)) return new Token(tokenStart, tokenStart)
    
    // regular token
    var separatorSeen = false
    while (zsciiChar != ZsciiEncoding.NullChar && !separatorSeen &&
           pos < bufferend) {
      pos += 1
      zsciiChar = ZsciiEncoding.zsciiCodeFor(state.byteAt(pos))
      if (isSpace(zsciiChar) || separators.exists(sep => sep == zsciiChar)) {
        separatorSeen = true
      }
    }
    new Token(tokenStart, pos - 1)
  }

  def process(input: String) {
    val version = storyVersion
    storeInputToTextBuffer(input)
    if (version < 5 || version >= 5 && parseBuffer > 0) {
      tokenize
    }
  }
  
}

