/*
 * Created on 2010/04/22
 * Copyright (c) 2010, Wei-ju Wu.
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

import org.specs._
import org.specs.matcher._
import org.specs.runner.{ConsoleRunner, JUnit4}

class MemoryHeapTest extends JUnit4(MemoryHeapSpec)
object MemoryHeapSpecRunner extends ConsoleRunner(MemoryHeapSpec)

object MemoryHeapSpec extends Specification with xUnit {
  "MemoryHeap" should {
    "be initialized" in {
      val heap = new MemoryHeap(1000)
      heap.address must_== 1000
    }
    "allocate a block of memory" in {
      val heap = new MemoryHeap(1000)
      val addr = heap.allocate(20)
      addr must be_>=(1000)
      heap.setByteAt(addr + 2, 42)
      heap.byteAt(addr + 2) must_== 42
      try {
        heap.setByteAt(addr * 2, 42)
        fail("accessing a non-existing memory area should throw an exception")
      } catch {
        case e:NullPointerException => assertTrue(true)
      }
    }
    "free a block of memory" in {
      val heap = new MemoryHeap(1000)
      val addr = heap.allocate(20)
      heap.free(addr)
      try {
        heap.setByteAt(addr, 42)
        fail("accessing a freed memory block should throw an exception")
      } catch {
        case e:NullPointerException => assertTrue(true)
      }
    }
    "allocate 3 blocks of memory" in {
      val heap = new MemoryHeap(1000)
      val addr1 = heap.allocate(20)
      val addr2 = heap.allocate(40)
      val addr3 = heap.allocate(60)
      heap.setByteAt(addr1 + 2, 42)
      heap.byteAt(addr1 + 2) must_== 42
      
      heap.setByteAt(addr2 + 4, 43)
      heap.byteAt(addr2 + 4) must_== 43
      
      heap.setByteAt(addr3 + 6, 44)
      heap.byteAt(addr3 + 6) must_== 44
    }
  }
}
