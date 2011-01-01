/*
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

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class MemoryTest extends JUnit4(MemorySpec)
object MemorySpecRunner extends ConsoleRunner(MemorySpec)

object MemorySpec extends Specification {

  "DefaultMemory" should {
    "be initialized" in {
      val data : Array[Byte] = Array(0x01, 0x02, 0x03, 0xff.toByte, 0xfe.toByte)
      val mem = new DefaultMemory(data)
      mem.byteAt(0)  must_== 1      
      mem.byteAt(1)  must_== 2      
      mem.byteAt(2)  must_== 3      
      mem.byteAt(3)  must_== 255
      mem.shortAt(3) must_== 0xfffe
      mem.intAt(0)   must_== 0x010203ff
    }
    "set/get" in {
      val data = new Array[Byte](10)
      val mem = new DefaultMemory(data)
      mem.setByteAt(0, 255)
      mem.byteAt(0) must_== 255
      mem.setShortAt(2, 65535)
      mem.shortAt(2) must_== 65535
      mem.setIntAt(4, 6553500)
      mem.intAt(4) must_== 6553500
    }
    "work with a offset" in {
      val data = new Array[Byte](10)
      val mem = new DefaultMemory(data, 10, 10, 0)
      mem.setByteAt(11, 255)
      mem.byteAt(11) must_== 255
    }
    // TODO: Tests for copying
  }
}

