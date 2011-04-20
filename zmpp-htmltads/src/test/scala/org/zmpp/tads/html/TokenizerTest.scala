/**
 * Created on 2011/04/16
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
package org.zmpp.tads.html

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._

@RunWith(classOf[JUnitRunner])
class TokenizerSpec extends FlatSpec with ShouldMatchers {

  "Tokenizer" should "emit start tokens" in {
    val reader = new StringReader("<html><body onload=\"bla\">")
    val tokenizer = new Tokenizer(reader)
    tokenizer.nextToken should be (StartTag("html", Map()))
    tokenizer.nextToken should be (StartTag("body", Map("onload" -> "bla")))
  }
  "Tokenizer" should "emit end tokens" in {
    val reader = new StringReader("</body></html>")
    val tokenizer = new Tokenizer(reader)
    tokenizer.nextToken should be (EndTag("body"))
    tokenizer.nextToken should be (EndTag("html"))
  }

  "Tokenizer" should "emit PCData" in {
    val reader = new StringReader("some text")
    val tokenizer = new Tokenizer(reader)
    tokenizer.nextToken should be (PCData("some text"))
    tokenizer.nextToken should be (EOF)
  }
  "Tokenizer" should "emit EOF" in {
    val reader = new StringReader("")
    val tokenizer = new Tokenizer(reader)
    tokenizer.nextToken should be (EOF)
  }
}
