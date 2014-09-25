/*
 * Created on 2011/10/05
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

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

class MockOutputStream extends OutputStream {
  val buffer = new StringBuilder
  def charsWritten = buffer.toString
  def putChar(c: Char) = buffer.append(c)
  def flush {}
  def select(flag: Boolean) { }
  def isSelected: Boolean = true
}

@RunWith(classOf[JUnitRunner])
class EncodingSpec extends FlatSpec with Matchers
with BeforeAndAfterEach {
  var out: MockOutputStream = null
  override def beforeEach {
    out = new MockOutputStream
  }

  "Encoding" should "have a defined initial state" in {
    val encoding = new ZsciiEncoding(new MockVMState(5, null))
    encoding.resetVMState
    encoding.currentAlphabet should be (Alphabet0)
  }
  it should "move to alphabet 1 on shift 4" in {
    val encoding = new ZsciiEncoding(new MockVMState(5, null))
    encoding.resetVMState
    encoding.decodeZchar(4, out)
    encoding.currentAlphabet should be (Alphabet1)
  }
  it should "move to alphabet 2 on shift 5" in {
    val encoding = new ZsciiEncoding(new MockVMState(5, null))
    encoding.resetVMState
    encoding.decodeZchar(5, out)
    encoding.currentAlphabet should be (Alphabet2)
  }
  it should "fall back to alphabet 0 on shift 4 after a character was decoded" in {
    val encoding = new ZsciiEncoding(new MockVMState(5, null))
    encoding.resetVMState
    encoding.decodeZchar(4, out)
    encoding.decodeZchar(6, out)
    out.charsWritten should be ("A")
    encoding.currentAlphabet should be (Alphabet0)
  }
  it should "fall back to alphabet 0 on shift 5 after a character was decoded" in {
    val encoding = new ZsciiEncoding(new MockVMState(5, null))
    encoding.resetVMState
    encoding.decodeZchar(5, out)
    encoding.decodeZchar(8, out)
    out.charsWritten should be ("0")
    encoding.currentAlphabet should be (Alphabet0)
  }

  // **********************************************************************
  // ******* Version 1/2 specific cases
  // **********************************************************************
  it should "fall back to alphabet 0 on shift 2 after a character was decoded" in {
    val encoding = new ZsciiEncoding(new MockVMState(2, null))
    encoding.resetVMState
    encoding.decodeZchar(2, out)
    encoding.decodeZchar(6, out)
    out.charsWritten should be ("A")
    encoding.currentAlphabet should be (Alphabet0)
  }
  it should "switch to alphabet 2 after receiving two shift-2's in a row and fallback to alphabet 1" in {
    val encoding = new ZsciiEncoding(new MockVMState(2, null))
    encoding.resetVMState
    encoding.decodeZchar(2, out)
    encoding.decodeZchar(2, out)
    encoding.currentAlphabet should be (Alphabet2)
    encoding.decodeZchar(8, out)
    out.charsWritten should be ("0")
    encoding.currentAlphabet should be (Alphabet1)
  }
  it should "fall back to alphabet 0 on shift 3 after a character was decoded" in {
    val encoding = new ZsciiEncoding(new MockVMState(1, null))
    encoding.resetVMState
    encoding.decodeZchar(3, out)
    encoding.decodeZchar(8, out)
    out.charsWritten should be ("1")
    encoding.currentAlphabet should be (Alphabet0)
  }
  it should "not fall back on shift 4 after a character was decoded in V1/V2" in {
    val encoding = new ZsciiEncoding(new MockVMState(1, null))
    encoding.resetVMState
    encoding.decodeZchar(4, out)
    encoding.decodeZchar(6, out)
    out.charsWritten should be ("A")
    encoding.currentAlphabet should be (Alphabet1)
  }
  it should "not fall back on shift 5 after a character was decoded in V2" in {
    val encoding = new ZsciiEncoding(new MockVMState(2, null))
    encoding.resetVMState
    encoding.decodeZchar(5, out)
    encoding.decodeZchar(8, out)
    out.charsWritten should be ("0")
    encoding.currentAlphabet should be (Alphabet2)
  }
  it should "not fall back on shift 5 after a character was decoded in V1" in {
    val encoding = new ZsciiEncoding(new MockVMState(1, null))
    encoding.resetVMState
    encoding.decodeZchar(5, out)
    encoding.decodeZchar(8, out)
    out.charsWritten should be ("1")
    encoding.currentAlphabet should be (Alphabet2_V1)
  }
  it should "reset a shift lock when a shift character comes after a lock-4" in {    
    val encoding = new ZsciiEncoding(new MockVMState(1, null))
    encoding.resetVMState
    encoding.decodeZchar(4, out)
    encoding.decodeZchar(2, out)
    encoding.decodeZchar(10, out)
    encoding.currentAlphabet should be (Alphabet1)
  }
  it should "reset a shift lock when a shift character comes after a lock-5" in {    
    val encoding = new ZsciiEncoding(new MockVMState(2, null))
    encoding.resetVMState
    encoding.decodeZchar(5, out)
    encoding.decodeZchar(3, out)
    encoding.decodeZchar(10, out)
    encoding.currentAlphabet should be (Alphabet2)
  }
  it should "reset its state properly after invoking reset" in {
    val encoding = new ZsciiEncoding(new MockVMState(2, null))
    encoding.resetVMState
    encoding.decodeZchar(2, out)
    encoding.decodeZchar(5, out)
    encoding.resetEncodingState
    encoding.currentAlphabet should be (Alphabet0)
    encoding.lastAlphabet should be (Alphabet0)
    encoding.shiftLock should be (false)
    encoding.decode10bit should be (false)
  }
}
