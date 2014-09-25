/*
 * Created on 2011/01/08
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
//
// Some dirty facts about T3 regexes:
//
// - regexes recognize the following special characters at the main level
//   of an expression:
//
//     '^' | '$' | '(' | ')' | '|' |  '<' | '%' | '.' | '['
//
//   This means
//
//     '*' | '+' | '?' | '{' | '}'
//
//   can appear at main level and are then recognized as a regular character.
//   These are only recognized in the "postfix" position state of the reference
//   parser.
//   Perl-style regexes might choke on this, because, as in T3, these are
//   special characters, but the parsers will treat them as such even when
//   in postfix position.
//   Since we translate to a Java regex (which is a Perl regex), we need
//   to explicitly escape said special characters when they are in the main
//   position. The closing mustache needs to be escaped when it closes
//   a non-postfix opening mustache.
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
                           "tab"      -> "\t",  "nul"      -> "\u0000",
                           "null"     -> "\u0000",
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

  private var parsePos    = 0
  private val builder       = new StringBuilder
  private var negate        = false
  private var caseSensitive = true

  def translate = {
    printf("RegexTranslator.translate('%s')\n", str)

    while (parsePos < str.length) {
      if (!handleRangeExpression) {
        val c = str.charAt(parsePos)
        if (isPunctuation(c)) handlePunctuation
        else {
          // this is the "non-postfix exception", escape the characters
          // that are special in postfix position to work around a
          // non-documented feature/bug in the reference implementation
          // which is used in at least one game (AllHope.t3)
          if (isPostfixSpecialChar(c)) builder.append("\\")
          builder.append(c)
          parsePos += 1
        }
      }
      // because of the "non-postfix exception", we have to
      // parse a postfix position in the translation mechanism.
      if (parsePos < str.length) handlePostfix
    }
    (caseSensitive, builder.toString)
  }

  private def handleRangeExpression = {
    if (str.charAt(parsePos) != '[') false
    else {
      var inRangeExpression = true
      while (inRangeExpression && parsePos < str.length) {
        val c = str.charAt(parsePos)
        builder.append(c)
        parsePos += 1
        if (c == ']') inRangeExpression = false
      }
      true
    }
  }

  private def isPunctuation(c: Char) = Punctuation.contains(c)

  private def handlePunctuation {
    val c = str.charAt(parsePos)
    if (c == '%') escapeCharacter
    else if (c == '<') angleExpression
    else {
      throw new UnsupportedOperationException("unsupported punctuation: '%c'\n".format(c))
    }
  }


  // handle the '%' character
  // in the regular case, this will simply replace '%' with '\\'
  // exceptions:
  // - '%<' and '%>' => '\\b' (Java/Perl do not support 'end of word')
  private def escapeCharacter {
    builder.append('\\')
    val nextChar = str.charAt(parsePos + 1)
    if (nextChar == '<' || nextChar == '>') builder.append('b')
    else builder.append(nextChar)
    parsePos += 2
  }

  // This is the part that differs the most from normal (Perl) regular
  // expressions. The rule is:
  // - between the angles comes a list of expressions, separated by '|'
  // - a '^' negates the content
  private def angleExpression {
    parsePos += 1
    negate = if (str.charAt(parsePos) == '^') {
      parsePos += 1
      true
    } else false
    val contentStringBuilder = new StringBuilder
    while (str.charAt(parsePos) != '>') {
      contentStringBuilder.append(str.charAt(parsePos))
      parsePos += 1
    }
    parsePos += 1 // skip the terminator '>'
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

  private def handlePostfix = {
    if (str.charAt(parsePos) == '{') {
      var inRepetitionDef = true
      while (inRepetitionDef && parsePos < str.length) {
        val c = str.charAt(parsePos)
        builder.append(c)
        if (c == '}') inRepetitionDef = false
        parsePos += 1
      }
    } else if (isPostfixSpecialCharPos1(str.charAt(parsePos))) {
      builder.append(str.charAt(parsePos))
      parsePos += 1
    }
  }

  // note that we exclude the '}' character here so we only handle
  // it either in non-postfix or ending a postfix expression
  private def isPostfixSpecialCharPos1(c: Char) = {
    c == '*' || c == '+' || c == '?' | c == '{'
  }

  private def isPostfixSpecialChar(c: Char) = {
    isPostfixSpecialCharPos1(c) || c == '}'
  }
}
