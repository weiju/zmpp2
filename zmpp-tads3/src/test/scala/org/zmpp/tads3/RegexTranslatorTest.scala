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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RegexTranslatorSpec extends FlatSpec with ShouldMatchers {

  "RegexTranslator" should "do a series of translations" in {
    ( new RegexTranslator("apattern").translate
     should equal ((true, "apattern")) )
    ( new RegexTranslator("%(apattern%)").translate
     should equal ((true, "\\(apattern\\)")) )
    ( new RegexTranslator("%<apattern%>").translate
     should equal ((true, "\\bapattern\\b")) )
  }
  it should "translate with nocase flag set" in {
    ( new RegexTranslator("apattern<NoCase>").translate
     should equal ((false, "apattern")) )
  }
  it should "translate with angle brackets" in {
    ( new RegexTranslator("<LANGLE>apattern<RAngle>").translate
     should equal ((true, "[<]apattern[>]")) )
    // mix in a nocase
    ( new RegexTranslator("<LANGLE>apattern<RAngle|nocase>").translate
     should equal ((false, "[<]apattern[>]")) )
  }
  it should "translate with square brackets" in {
    val pat = new RegexTranslator("<lSquare>apattern<rsquare>").translate
    pat should equal ((true, "[\\[]apattern[\\]]"))
    "[apattern]".matches(pat._2) should be (true)
  }
  it should "translate with curly braces" in {
    ( new RegexTranslator("<lbrace>apattern<rbrace>").translate
     should equal ((true, "[{]apattern[}]")) )
  }
  it should "translate with vertical bar" in {
    ( new RegexTranslator("apattern<vbar>").translate
     should equal ((true, "apattern[|]")) )
  }
  it should "translate with caret" in {
    ( new RegexTranslator("apattern<caret>").translate
     should equal ((true, "apattern[\\^]")) )
  }
  it should "translate with single quotes" in {
    ( new RegexTranslator("<squote>apattern<squote>").translate
     should equal ((true, "[']apattern[']")) )
  }
  it should "translate with double quotes" in {
    val pat = new RegexTranslator("<dquote>apattern<dquote>").translate
    pat should equal ((true, "[\"]apattern[\"]"))
    "\"apattern\"".matches(pat._2) should be (true)
  }
  it should "translate with star" in {
    val pat = new RegexTranslator("apattern<star>").translate
    pat should equal ((true, "apattern[*]"))
    "apattern*".matches(pat._2) should be (true)
  }
  it should "translate with plus" in {
    val pat = new RegexTranslator("apattern<plus>").translate
    pat should equal ((true, "apattern[+]"))
    "apattern+".matches(pat._2) should be (true)
  }
  it should "translate with percent" in {
    val pat = new RegexTranslator("apattern<percent>").translate
    pat should equal ((true, "apattern[%]"))
    "apattern%".matches(pat._2) should be (true)
  }
  it should "translate with question mark" in {
    val pat = new RegexTranslator("apattern<question>").translate
    pat should equal ((true, "apattern[?]"))
    "apattern?".matches(pat._2) should be (true)
  }
  it should "translate with dollar" in {
    val pat = new RegexTranslator("apattern<dollar>").translate
    pat should equal ((true, "apattern[$]"))
    "apattern$".matches(pat._2) should be (true)
  }
  it should "translate with backslash" in {
    val pat = new RegexTranslator("apattern<backslash>").translate
    pat should equal ((true, "apattern[\\\\]"))
    "apattern\\".matches(pat._2) should be (true)
  }
  it should "translate with return" in {
    val pat = new RegexTranslator("apattern<return>").translate
    pat should equal ((true, "apattern[\r]"))
    "apattern\r".matches(pat._2) should be (true)
  }
  it should "translate with linefeed" in {
    val pat = new RegexTranslator("apattern<linefeed>").translate
    pat should equal ((true, "apattern[\n]"))
    "apattern\n".matches(pat._2) should be (true)
  }
  it should "translate with tab" in {
    val pat = new RegexTranslator("apattern<tab>").translate
    pat should equal ((true, "apattern[\t]"))
    "apattern\t".matches(pat._2) should be (true)
  }
  it should "translate with nul" in {
    val pat = new RegexTranslator("apattern<nul>").translate
    pat should equal ((true, "apattern[\0]"))
    "apattern\0".matches(pat._2) should be (true)
  }
  it should "translate with null" in {
    val pat = new RegexTranslator("apattern<null>").translate
    pat should equal ((true, "apattern[\0]"))
    "apattern\0".matches(pat._2) should be (true)
  }
  it should "translate with alphanum" in {
    val pat1 = new RegexTranslator("apattern<alphanum>").translate
    pat1 should equal ((true, "apattern[\\p{Alnum}]"))
    "apattern2".matches(pat1._2) should be (true)
    val pat2 = new RegexTranslator("apattern<^alphanum>").translate
    pat2 should equal ((true, "apattern[^\\p{Alnum}]"))
    "apattern\t".matches(pat2._2) should be (true)
  }
  it should "translate with upper" in {
    val pat = new RegexTranslator("apattern<upper>").translate
    pat should equal ((true, "apattern[\\p{Upper}]"))
    "apatternJ".matches(pat._2) should be (true)
  }
  it should "translate with lower" in {
    val pat = new RegexTranslator("apattern<lower>").translate
    pat should equal ((true, "apattern[\\p{Lower}]"))
    "apatternj".matches(pat._2) should be (true)
  }
  it should "translate with alpha" in {
    val pat = new RegexTranslator("apattern<alpha>").translate
    pat should equal ((true, "apattern[\\p{Alpha}]"))
    "apatternj".matches(pat._2) should be (true)
  }
  it should "translate with digit" in {
    val pat = new RegexTranslator("apattern<digit>").translate
    pat should equal ((true, "apattern[\\p{Digit}]"))
    "apattern3".matches(pat._2) should be (true)
  }
  it should "translate with space" in {
    val pat = new RegexTranslator("apattern<space>").translate
    pat should equal ((true, "apattern[\\p{Space}]"))
    "apattern\t".matches(pat._2) should be (true)
  }
  it should "translate with punct" in {
    val pat = new RegexTranslator("apattern<punct>").translate
    pat should equal ((true, "apattern[\\p{Punct}]"))
    "apattern!".matches(pat._2) should be (true)
  }
  it should "translate with newline" in {
    val pat = new RegexTranslator("apattern<newline>").translate
    pat should equal ((true, "apattern[\r\n\u2028]"))
    "apattern\r".matches(pat._2) should be (true)
    "apattern\n".matches(pat._2) should be (true)
    "apattern\u2028".matches(pat._2) should be (true)
  }

  it should "translate something harder" in {
    ( new RegexTranslator("<nocase><langle>%.(/?[a-z][a-z0-9]*)<rangle>").translate
     should equal ((false, "[<]\\.(/?[a-z][a-z0-9]*)[>]")) )
    ( new RegexTranslator("(<langle><dot>[pP]0?<rangle>)+").translate
     should equal ((true, "([<][.][pP]0?[>])+")) )
    ( new RegexTranslator("[.;:!?]<^alphanum>").translate
     should equal ((true, "[.;:!?][^\\p{Alnum}]")) )
  }

  // regressions and special cases
  it should "translate with literal character (regression 1)" in {
    val pat = new RegexTranslator(
      "(<^space|/>+)<space>+(<^space|/>+)(/<^space|/>+)").translate
    pat should equal ((true, "([^\\p{Space}/]+)[\\p{Space}]+([^\\p{Space}/]+)(/[^\\p{Space}/]+)"))
  }

  it should "translate with angle bracket in range (regression 2)" in {
    new RegexTranslator("[<\"']").translate should equal ((true, "[<\"']"))
    new RegexTranslator("[><%]").translate  should equal ((true, "[><%]"))
  }

  it should "translate with mustache in non-postfix position" in {
    new RegexTranslator("{[^}]+<squote>[^}]*}").translate should equal ((true, "\\{[^}]+['][^}]*\\}"))
  }
}
