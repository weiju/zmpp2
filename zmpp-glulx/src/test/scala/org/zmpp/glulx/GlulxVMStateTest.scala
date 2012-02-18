/*
 * Created on 2010/05/06
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
package org.zmpp.glulx

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

import org.zmpp.base._

@RunWith(classOf[JUnitRunner])
class GlulxVMStackOperationsSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val DummyMem = Array[Byte](0x47, 0x6c, 0x75, 0x6c, 0x00, 0x03, 0x01, 0x01,
                             0x00, 0x00, 0x00, 0x00, // RAMSTART
                             0x00, 0x00, 0x00, 0x00, // EXTSTART
                             0x00, 0x00, 0x00, 0x00, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x00, // STARTFUNC
                             0x00, 0x00, 0x00, 0x00, // Decoding table
                             0x00, 0x00, 0x00, 0x00 // Checksum
                            )
  val vm = new GlulxVM()

  override def beforeEach {
    // push artificial call frame
    vm.initState(DummyMem)
    vm.pushInt(12)
    vm.pushInt(12)
    vm.pushInt(0)
  }

  "GlulxVM" should "do StkSwap" in {
    vm.pushInt(1)
    vm.pushInt(2)
    vm.pushInt(3)

    // after stack swap, order must be 2, 3, 1
    vm.stackSwap
    vm.popInt should equal (2)
    vm.popInt should equal (3)
    vm.popInt should equal (1)
  }
  it should "do StkPeek" in {
    vm.pushInt(1)
    vm.pushInt(2)
    vm.pushInt(3)
      
    vm.stackPeek(0) should equal (3)
    vm.stackPeek(1) should equal (2)
    vm.stackPeek(2) should equal (1)
  }
  it should "do StkRoll with positive rotate" in {
    vm.pushInt(8)
    vm.pushInt(7)
    vm.pushInt(6)
    vm.pushInt(5)
    vm.pushInt(4)
    vm.pushInt(3)
    vm.pushInt(2)
    vm.pushInt(1)
    vm.pushInt(0)
      
    vm.stackRoll(5, 1)
    vm.popInt should equal (1)
    vm.popInt should equal (2)
    vm.popInt should equal (3)
    vm.popInt should equal (4)
    vm.popInt should equal (0)
  }
  it should "do StkRoll with negative rotate" in {
    vm.pushInt(8)
    vm.pushInt(7)
    vm.pushInt(6)
    vm.pushInt(5)
    vm.pushInt(4)
    vm.pushInt(3)
    vm.pushInt(2)
    vm.pushInt(1)
    vm.pushInt(0)
      
    vm.stackRoll(5, 1)
    vm.stackRoll(9, -3)
    vm.popInt should equal (6)
    vm.popInt should equal (7)
    vm.popInt should equal (8)
    vm.popInt should equal (1)
    vm.popInt should equal (2)
    vm.popInt should equal (3)
    vm.popInt should equal (4)
    vm.popInt should equal (0)
    vm.popInt should equal (5)
  }
}

@RunWith(classOf[JUnitRunner])
class GlulxVMInitSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val DummyMem = Array[Byte](0x47, 0x6c, 0x75, 0x6c,
                             0x00, 0x03, 0x01, 0x01, // Version
                             0x00, 0x00, 0x00, 0x24, // RAMSTART
                             0x00, 0x00, 0x00, 0x28, // EXTSTART
                             0x00, 0x00, 0x00, 0x28, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x00, // STARTFUNC
                             0x04, 0x07, 0x01, 0x01, // Decoding table
                             0x01, 0x02, 0x03, 0x04, // Checksum
                             0x00, 0x00, 0x00, 0x00  // simulated RAM (0x24 = 36 dec)
                            )
  var vm = new GlulxVM()

  "GlulxVM" should "be in a defined state after initState()" in {
    vm.initState(DummyMem)
    vm.isGlulx    should be (true)
    vm.pc         should equal (0)
    vm.fp         should equal (0)
    vm.version    should equal (0x00030101)
    vm.ramstart   should equal (0x24)
    vm.extstart   should equal (0x28)
    vm.endmem     should equal (0x28)
    vm.stacksize  should equal (255)
    vm.startfunc  should equal (0x00)
    vm.checksum   should equal (0x01020304)
    vm.runState   should equal (VMRunStates.Running)
    vm.memsize    should equal (0x28)
    vm.currentDecodingTable should equal (0x04070101)
  }
}
