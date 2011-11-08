/**
 * Created on 2011/11/07
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
package org.zmpp.iff

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QuetzalCompressionSpec extends FlatSpec with ShouldMatchers {

  "QuetzalCompression" should "compress an array with no changes" in {
    val originalBytes: Array[Byte] = Array(1, 2, 3, 4)
    val saveBytes: Array[Byte] = Array(1, 2, 3, 4)
    val compressed = QuetzalCompression.compressDiffBytes(originalBytes, saveBytes, 4)
    compressed.length should be (2)
    compressed(0) should be (0)
    compressed(1) should be (3)
  }
  it should "compress an array with one change at the end" in {
    val originalBytes = new Array[Byte](300)
    val saveBytes = new Array[Byte](300)
    for (i <- 0 until 300) originalBytes(i) = (i % 256).asInstanceOf[Byte]
    System.arraycopy(originalBytes, 0, saveBytes, 0, 300)
    saveBytes(299) = 0x28
    val compressed = QuetzalCompression.compressDiffBytes(originalBytes, saveBytes, 300)
    compressed.length should be (5)
    compressed(0) should be (0)
    (compressed(1) & 0xff) should be (255)
    compressed(2) should be (0)
    compressed(3) should be (42)
    compressed(4) should be (3)
  }

  it should "decompress an array with no changes" in {
    val originalBytes: Array[Byte] = Array(1, 2, 3, 4)
    val compressed: Array[Byte] = Array(0, 3)
    val result = QuetzalCompression.decompressDiffBytes(compressed, originalBytes, 4)
    result.length should be (4)
    result(0) should be (1)
    result(1) should be (2)
    result(2) should be (3)
    result(3) should be (4)
  }

  it should "decompress an array with one change at the end" in {
    val originalBytes = new Array[Byte](300)
    for (i <- 0 until 300) originalBytes(i) = (i % 256).asInstanceOf[Byte]
    val compressed: Array[Byte] = Array(0, 255.asInstanceOf[Byte], 0, 42, 3)
    val result = QuetzalCompression.decompressDiffBytes(compressed, originalBytes, 300)
    result(299) should be (0x28)
  }
}
