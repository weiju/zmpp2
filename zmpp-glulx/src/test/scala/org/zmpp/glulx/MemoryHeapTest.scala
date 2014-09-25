/*
 * Created on 2010/04/22
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
package org.zmpp.glulx

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MemoryHeapSpec extends FlatSpec with Matchers {

  "MemoryHeap" should "be initialized" in {
    val heap = new MemoryHeap(1000)
    heap.address should equal (1000)
  }
  it should "allocate a block of memory" in {
    val heap = new MemoryHeap(1000)
    val addr = heap.allocate(20)
    addr should be >= (1000)

    heap.setByteAt(addr + 2, 42)
    heap.byteAt(addr + 2) should equal (42)
    a [NullPointerException] should be thrownBy { heap.setByteAt(addr * 2, 42) }
  }
  it should "free a block of memory" in {
    val heap = new MemoryHeap(1000)
    val addr = heap.allocate(20)
    heap.free(addr)
    a [NullPointerException] should be thrownBy { heap.setByteAt(addr, 42) }
  }
  it should "allocate 3 blocks of memory" in {
    val heap = new MemoryHeap(1000)
    val addr1 = heap.allocate(20)
    val addr2 = heap.allocate(40)
    val addr3 = heap.allocate(60)

    heap.setByteAt(addr1 + 2, 42)
    heap.byteAt(addr1 + 2) should equal (42)
      
    heap.setByteAt(addr2 + 4, 43)
    heap.byteAt(addr2 + 4) should equal (43)
      
    heap.setByteAt(addr3 + 6, 44)
    heap.byteAt(addr3 + 6) should equal (44)
  }
}
