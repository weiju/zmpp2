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
import org.zmpp.base.Memory

@RunWith(classOf[JUnitRunner])
class CustomAccentTableSpec extends FlatSpec with Matchers {
  object MockMem extends Memory {
    val Data = Array[Byte](0x03, 0x04, 0x01, 0x04, 0x02, 0x04, 0x03)
    def size = Data.length
    def buffer = Data
    def byteAt(addr: Int) = Data(addr)
    def shortAt(addr: Int) = {
      ((Data(addr) & 0xff) << 8 | (Data(addr + 1) & 0xff))
    }
    def intAt(addr: Int) = throw new UnsupportedOperationException()
    def setByteAt(addr: Int, value: Int) {}
    def setShortAt(addr: Int, value: Int) {}
    def setIntAt(addr: Int, value: Int) {}
    def copyBytesTo(dest: Array[Byte], srcOffest: Int, numBytes: Int) {}
    def copyBytesTo(dstOffset: Int, srcOffest: Int, numBytes: Int) {}
    def copyBytesFrom(src: Array[Byte], srcOffset: Int, destOffset: Int, numBytes: Int) {}
  }

  "CustomAccentTable" should "lookup an accent" in {
    val accentTable = new CustomAccentTable(MockMem, 0)
    accentTable.numWords should be (3)
    accentTable(0) should be (0x401.asInstanceOf[Char])
    accentTable(1) should be (0x402.asInstanceOf[Char])
    accentTable(2) should be (0x403.asInstanceOf[Char])
  }
  it should "return the index of an accent" in {
    val accentTable = new CustomAccentTable(MockMem, 0)
    accentTable.indexOf(0x401.asInstanceOf[Char]) should be (0)
    accentTable.indexOf(0x402.asInstanceOf[Char]) should be (1)
    accentTable.indexOf(0x403.asInstanceOf[Char]) should be (2)
  }
}
