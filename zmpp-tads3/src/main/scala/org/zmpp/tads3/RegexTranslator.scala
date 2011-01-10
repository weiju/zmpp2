/*
 * Created on 2011/01/08
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
package org.zmpp.tads3

import java.util.regex._
import scala.collection.JavaConversions._

// I really do not want to write my own regex library, the one Java follows
// Perl and is therefore good enough in 99.999999% of all cases.
// Instead, we translate a T3 regex into a Perl regex and let the Java
// regex library handle the rest.
// Angle bracket expressions are replaced with range expressions and using
// POSIX equivalents where possible.
//
// Note that there are some things that do not map to a Perl/Java regext 1:1:
// - flags (case/nocase, min/max, firstbegin/firstend)
// - end-of-word match (the normal word boundary match is used instead)
//
// TODO:
// - group match expressions ('%1', '%2' etc), will be mapped to back references
// - lookahead/non-capturing group (might be already working, looks compatible to Perl)
// - ranges with special characters
//
// Here I wanted to say that I used the cool Scala parser combinator library,
// but I did not, it seems more efficient and simple to just do a straightforward
// implementation

object RegexTranslator {
  val Punctuation = Set('%', '<', '>' )
  val SymbolMappings = Map("langle"   -> "<",   "rangle"    -> ">",
                           "lsquare"  -> "\\[", "rsquare"   -> "\\]",
                           "lbrace"   -> "{",   "rbrace"    -> "}",
                           "dot"      -> ".",   "period"    -> ".",
                           "vbar"     -> "|",   "caret"     -> "\\^",
                           "squote"   -> "'",   "dquote"    -> "\"",
                           "star"     -> "*",   "plus"      -> "+",
                           "percent"  -> "%",   "question"  -> "?",
                           "dollar"   -> "$",   "backslash" -> "\\\\",
                           "return"   -> "\r",  "linefeed" -> "\n",
                           "tab"      -> "\t",  "nul"      -> "\0",
                           "null"     -> "\0",
                           "alpha"    -> "\\p{Alpha}",
                           "upper"    -> "\\p{Upper}",
                           "lower"    -> "\\p{Lower}",
                           "digit"    -> "\\p{Digit}",
                           "alphanum" -> "\\p{Alnum}",
                           "space"    -> "\\p{Space}",
                           "punct"    -> "\\p{Punct}",
                           "newline"  -> "\r\n\u2028"
                         )
}

class RegexTranslator(str: String) {
  import RegexTranslator._

  private var i             = 0
  private val builder       = new StringBuilder
  private var negate        = false
  private var caseSensitive = true

  private def isPunctuation(c: Char) = Punctuation.contains(c)

  // handle the '%' character
  // in the regular case, this will simply replace '%' with '\\'
  // exceptions:
  // - '%<' and '%>' => '\\b' (Java/Perl do not support 'end of word')
  private def escapeCharacter {
    builder.append('\\')
    val nextChar = str.charAt(i + 1)
    if (nextChar == '<' || nextChar == '>') builder.append('b')
    else builder.append(nextChar)
    i += 2
  }

  // This is the part that differs the most from normal (Perl) regular
  // expressions. The rule is:
  // - between the angles comes a list of expressions, separated by '|'
  // - a '^' negates the content
  private def angleExpression {
    i += 1
    negate = if (str.charAt(i) == '^') {
      i += 1
      true
    } else false
    val contentStringBuilder = new StringBuilder
    while (str.charAt(i) != '>') {
      contentStringBuilder.append(str.charAt(i))
      i += 1
    }
    i += 1 // skip the terminator '>'
    val comps = contentStringBuilder.toString.split('|')
    var identifiers = comps.map(comp => if (comp.contains("-")) comp.trim
                                        else comp.trim.toLowerCase)
    // if there is any nocase in here, we set the whole pattern to
    // non-sensitive, default is case sensitive
    if (identifiers.filter(_ == "nocase").length > 0) caseSensitive = false

    // filter out the case modifiers, since there is no text substitution for
    // those
    identifiers = identifiers.filter(id => id != "nocase" && id != "case")
    if (identifiers.length > 0) {
      builder.append("[")
      if (negate) builder.append("^")
      identifiers.foreach(str => {
        if (SymbolMappings.contains(str)) builder.append(SymbolMappings(str))
        else builder.append(str)
      })
      builder.append("]")
    }
  }

  private def handlePunctuation {
    val c = str.charAt(i)
    if (c == '%') escapeCharacter
    else if (c == '<') angleExpression
    else {
      throw new UnsupportedOperationException("unsupported punctuation: '%c'\n".format(c))
    }
  }

  def translate = {
    printf("RegexTranslator.translate('%s')\n", str)
    while (i < str.length) {
      val c = str.charAt(i)
      if (isPunctuation(c)) handlePunctuation
      else {
        builder.append(c)
        i += 1
      }
    }
    (caseSensitive, builder.toString)
  }
}
