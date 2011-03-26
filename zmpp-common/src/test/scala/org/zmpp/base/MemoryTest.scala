/**
 * Created on 2010/04/01
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
package org.zmpp.base

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._

@RunWith(classOf[JUnitRunner])
class MemorySpec extends FlatSpec with ShouldMatchers {

  "DefaultMemory" should "be initialized" in {
    val data : Array[Byte] = Array(0x01, 0x02, 0x03, 0xff.toByte, 0xfe.toByte)
    val mem = new DefaultMemory(data)
    mem.byteAt(0)  should be (1)
    mem.byteAt(1)  should be (2)
    mem.byteAt(2)  should be (3)
    mem.byteAt(3)  should be (255)
    mem.shortAt(3) should be (0xfffe)
    mem.intAt(0)   should be (0x010203ff)
  }
  it should  "set/get" in {
    val data = new Array[Byte](10)
    val mem = new DefaultMemory(data)

    mem.setByteAt(0, 255)
    mem.setShortAt(2, 65535)
    mem.setIntAt(4, 6553500)

    mem.byteAt(0) should be (255)
    mem.shortAt(2) should be (65535)
    mem.intAt(4) should be (6553500)
  }
  it should "work with a offset" in {
    val data = new Array[Byte](10)
    val mem = new DefaultMemory(data, 10, 10, 0)
    mem.setByteAt(11, 255)

    mem.byteAt(11) should be (255)
  }
  // TODO: Tests for copying
}

