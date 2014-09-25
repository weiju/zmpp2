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

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RegexTranslatorSpec extends FlatSpec {

  "RegexTranslator" should "do a series of translations" in {
    assert( new RegexTranslator("apattern").translate == (true, "apattern"))
    assert( new RegexTranslator("%(apattern%)").translate == (true, "\\(apattern\\)"))
    assert( new RegexTranslator("%<apattern%>").translate == (true, "\\bapattern\\b"))
  }
  it should "translate with nocase flag set" in {
    assert( new RegexTranslator("apattern<NoCase>").translate ==  (false, "apattern"))
  }
  it should "translate with angle brackets" in {
    assert( new RegexTranslator("<LANGLE>apattern<RAngle>").translate == (true, "[<]apattern[>]"))
    // mix in a nocase
    assert( new RegexTranslator("<LANGLE>apattern<RAngle|nocase>").translate == (false, "[<]apattern[>]"))
  }
  it should "translate with square brackets" in {
    val pat = new RegexTranslator("<lSquare>apattern<rsquare>").translate
    assert(pat == (true, "[\\[]apattern[\\]]"))
    assert("[apattern]".matches(pat._2))
  }
  it should "translate with curly braces" in {
    assert( new RegexTranslator("<lbrace>apattern<rbrace>").translate == (true, "[{]apattern[}]"))
  }
  it should "translate with vertical bar" in {
    assert( new RegexTranslator("apattern<vbar>").translate == (true, "apattern[|]"))
  }
  it should "translate with caret" in {
    assert( new RegexTranslator("apattern<caret>").translate == (true, "apattern[\\^]"))
  }
  it should "translate with single quotes" in {
    assert( new RegexTranslator("<squote>apattern<squote>").translate == (true, "[']apattern[']"))
  }
  it should "translate with double quotes" in {
    val pat = new RegexTranslator("<dquote>apattern<dquote>").translate
    assert(pat == (true, "[\"]apattern[\"]"))
    assert("\"apattern\"".matches(pat._2))
  }
  it should "translate with star" in {
    val pat = new RegexTranslator("apattern<star>").translate
    assert(pat == (true, "apattern[*]"))
    assert("apattern*".matches(pat._2))
  }
  it should "translate with plus" in {
    val pat = new RegexTranslator("apattern<plus>").translate
    assert(pat == (true, "apattern[+]"))
    assert("apattern+".matches(pat._2))
  }
  it should "translate with percent" in {
    val pat = new RegexTranslator("apattern<percent>").translate
    assert(pat == (true, "apattern[%]"))
    assert("apattern%".matches(pat._2))
  }
  it should "translate with question mark" in {
    val pat = new RegexTranslator("apattern<question>").translate
    assert(pat == (true, "apattern[?]"))
    assert("apattern?".matches(pat._2))
  }
  it should "translate with dollar" in {
    val pat = new RegexTranslator("apattern<dollar>").translate
    assert(pat == (true, "apattern[$]"))
    assert("apattern$".matches(pat._2))
  }
  it should "translate with backslash" in {
    val pat = new RegexTranslator("apattern<backslash>").translate
    assert(pat == (true, "apattern[\\\\]"))
    assert("apattern\\".matches(pat._2))
  }
  it should "translate with return" in {
    val pat = new RegexTranslator("apattern<return>").translate
    assert(pat == (true, "apattern[\r]"))
    assert("apattern\r".matches(pat._2))
  }
  it should "translate with linefeed" in {
    val pat = new RegexTranslator("apattern<linefeed>").translate
    assert(pat == (true, "apattern[\n]"))
    assert("apattern\n".matches(pat._2))
  }
  it should "translate with tab" in {
    val pat = new RegexTranslator("apattern<tab>").translate
    assert(pat == (true, "apattern[\t]"))
    assert("apattern\t".matches(pat._2))
  }
  it should "translate with nul" in {
    val pat = new RegexTranslator("apattern<nul>").translate
    assert(pat == (true, "apattern[\u0000]"))
    assert("apattern\u0000".matches(pat._2))
  }
  it should "translate with null" in {
    val pat = new RegexTranslator("apattern<null>").translate
    assert(pat == (true, "apattern[\u0000]"))
    assert("apattern\u0000".matches(pat._2))
  }
  it should "translate with alphanum" in {
    val pat1 = new RegexTranslator("apattern<alphanum>").translate
    assert(pat1 == (true, "apattern[\\p{Alnum}]"))
    assert("apattern2".matches(pat1._2))
    val pat2 = new RegexTranslator("apattern<^alphanum>").translate
    assert(pat2 == (true, "apattern[^\\p{Alnum}]"))
    assert("apattern\t".matches(pat2._2))
  }
  it should "translate with upper" in {
    val pat = new RegexTranslator("apattern<upper>").translate
    assert(pat == (true, "apattern[\\p{Upper}]"))
    assert("apatternJ".matches(pat._2))
  }
  it should "translate with lower" in {
    val pat = new RegexTranslator("apattern<lower>").translate
    assert(pat == (true, "apattern[\\p{Lower}]"))
    assert("apatternj".matches(pat._2))
  }
  it should "translate with alpha" in {
    val pat = new RegexTranslator("apattern<alpha>").translate
    assert(pat == (true, "apattern[\\p{Alpha}]"))
    assert("apatternj".matches(pat._2))
  }
  it should "translate with digit" in {
    val pat = new RegexTranslator("apattern<digit>").translate
    assert(pat == (true, "apattern[\\p{Digit}]"))
    assert("apattern3".matches(pat._2))
  }
  it should "translate with space" in {
    val pat = new RegexTranslator("apattern<space>").translate
    assert(pat == (true, "apattern[\\p{Space}]"))
    assert("apattern\t".matches(pat._2))
  }
  it should "translate with punct" in {
    val pat = new RegexTranslator("apattern<punct>").translate
    assert(pat == (true, "apattern[\\p{Punct}]"))
    assert("apattern!".matches(pat._2))
  }
  it should "translate with newline" in {
    val pat = new RegexTranslator("apattern<newline>").translate
    assert(pat == (true, "apattern[\r\n\u2028]"))
    assert("apattern\r".matches(pat._2))
    assert("apattern\n".matches(pat._2))
    assert("apattern\u2028".matches(pat._2))
  }

  it should "translate something harder" in {
    assert( new RegexTranslator("<nocase><langle>%.(/?[a-z][a-z0-9]*)<rangle>").translate ==
      (false, "[<]\\.(/?[a-z][a-z0-9]*)[>]"))
    assert( new RegexTranslator("(<langle><dot>[pP]0?<rangle>)+").translate ==
     (true, "([<][.][pP]0?[>])+"))
    assert( new RegexTranslator("[.;:!?]<^alphanum>").translate ==
     (true, "[.;:!?][^\\p{Alnum}]"))
  }

  // regressions and special cases
  it should "translate with literal character (regression 1)" in {
    val pat = new RegexTranslator(
      "(<^space|/>+)<space>+(<^space|/>+)(/<^space|/>+)").translate
    assert(pat == (true, "([^\\p{Space}/]+)[\\p{Space}]+([^\\p{Space}/]+)(/[^\\p{Space}/]+)"))
  }

  it should "translate with angle bracket in range (regression 2)" in {
    assert(new RegexTranslator("[<\"']").translate == (true, "[<\"']"))
    assert(new RegexTranslator("[><%]").translate == (true, "[><%]"))
  }

  it should "translate with mustache in non-postfix position" in {
    assert(new RegexTranslator("{[^}]+<squote>[^}]*}").translate == (true, "\\{[^}]+['][^}]*\\}"))
  }
}
