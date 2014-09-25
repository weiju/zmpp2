/**
 * Created on 2011/04/16
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
package org.zmpp.tads.html

// note that we need to qualify StringBuilder, because
// scala.collection.mutable.StringBuilder, which would be
// used otherwise, does not support appendCodePoint()
import java.lang.StringBuilder

import java.io.Reader
import java.io.PushbackReader

import scala.collection.mutable.HashMap

trait Token {
}
case class PCData(text: String) extends Token
case object EOF extends Token
case class StartTag(name: String, attributes: Map[String, String]) extends Token
case class EndTag(name: String) extends Token

/**
 * A HTML TADS Tokenizer. It reads a stream of characters and
 * spits out HTML tokens.
 * These can be:
 * - Tags
 * - PCDATA
 */
class Tokenizer(reader: Reader) {
  val input = new PushbackReader(reader)

  trait TokenizerState {
    /**
     * Returns (tokenParsed, nextState)
     */
    def processChars: (Boolean, TokenizerState)
  }
  object StartTokenize extends TokenizerState {
    def processChars = {
      val c = nextChar
      if (c == -1) {
        currentToken = EOF
        (true, StopTokenize)
      } else if (c == '<') {
        (false, TokenizeTag)
      } else {
        putbackChar(c)
        (false, ReadPCData)
      }
    }
  }
  object ReadPCData extends TokenizerState {
    def processChars: (Boolean, TokenizerState) = {
      var c = nextChar
      val builder = new StringBuilder
      while (c != '<' && c != -1) {
        builder.appendCodePoint(c)
        c = nextChar
      }
      currentToken = PCData(builder.toString)
      (true, StartTokenize)
    }
  }
  
  object TokenizeTag extends TokenizerState {
    def processChars = {
      val c = nextChar
      if (c == '/') (false, TokenizeEndTag)
      else {
        putbackChar(c)
        (false, TokenizeStartTag)
      }
    }
  }
  object TokenizeStartTag extends TokenizerState {
    def processChars = {
      var c = nextChar
      val builder = new StringBuilder
      while (c != '>') {
        builder.appendCodePoint(c)
        c = nextChar
      }
      currentToken = buildStartTag(builder.toString)
      (true, StartTokenize)
    }
    private def buildStartTag(str: String) = {
      val comps = str.split("\\s")
      val name = comps(0)
      val attributeMap = new HashMap[String, String]
      for (i <- 1 until comps.length) {
        processAttribute(attributeMap, comps(i))
      }
      StartTag(name, attributeMap.toMap)
    }
    private def processAttribute(attributeMap: HashMap[String, String],
                                 attrString: String) = {
      val attrComps = attrString.split("=")
      val key = attrComps(0)
      var value = if (attrComps.length > 1) attrComps(1) else null
      attributeMap(key) = unquote(value)
    }
    private def unquote(str: String): String = {
      var result = str;
      if (str != null) {
        if (result.charAt(0) == '\'' ||
          result.charAt(0) == '"') result = result.substring(1)
        if (result.charAt(result.length - 1) == '\'' ||
            result.charAt(result.length - 1) == '"')
          result = result.substring(0, result.length - 1)
      }
      result
    }
  }
  object TokenizeEndTag extends TokenizerState {
    def processChars = {
      var c = nextChar
      val builder = new StringBuilder
      while (c != '>') {
        builder.appendCodePoint(c)
        c = nextChar
      }
      currentToken = EndTag(builder.toString)
      (true, StartTokenize)
    }
  }

  object StopTokenize extends TokenizerState {
    def processChars = {
      currentToken = EOF
      (true, StopTokenize)
    }
  }

  var position = 0
  var currentToken: Token = null
  var currentState: TokenizerState = StartTokenize

  def nextToken: Token = {
    var status = currentState.processChars
    while (!status._1) {
      currentState = status._2
      status = currentState.processChars
    }
    currentState = StartTokenize
    currentToken
  }

  private def nextChar: Int = {
    val result = input.read
    if (result != -1) {
      position += 1
    }
    result
  }
  private def putbackChar(c: Int) = {
    input.unread(c)
    position -= 1
  }
}
