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
class GlulxVMStateSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val DummyMem = Array[Byte](0x47, 0x6c, 0x75, 0x6c, 0x00, 0x03, 0x01, 0x01,
                             0x00, 0x00, 0x00, 0x00, // RAMSTART
                             0x00, 0x00, 0x00, 0x00, // EXTSTART
                             0x00, 0x00, 0x00, 0x00, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x00, // STARTFUNC
                             0x00, 0x00, 0x00, 0x00, // Decoding table
                             0x00, 0x00, 0x00, 0x00 // Checksum
                            )
  var vmstate = new GlulxVM()

  override def beforeEach {
    // push artificial call frame
    vmstate.init(DummyMem, null)
    vmstate.pushInt(12)
    vmstate.pushInt(12)
    vmstate.pushInt(0)
  }

  "GlulxVMStateSpec" should "be initialized" in {
    vmstate.pc should equal (0)
    vmstate.fp should equal (0)
    vmstate.sp should equal (12)
  }
  it should "do StkSwap" in {
    vmstate.pushInt(1)
    vmstate.pushInt(2)
    vmstate.pushInt(3)

    // after stack swap, order must be 2, 3, 1
    vmstate.stackSwap
    vmstate.popInt should equal (2)
    vmstate.popInt should equal (3)
    vmstate.popInt should equal (1)
  }
  it should "do StkPeek" in {
    vmstate.pushInt(1)
    vmstate.pushInt(2)
    vmstate.pushInt(3)
      
    vmstate.stackPeek(0) should equal (3)
    vmstate.stackPeek(1) should equal (2)
    vmstate.stackPeek(2) should equal (1)
  }
  it should "do StkRoll with positive rotate" in {
    vmstate.pushInt(8)
    vmstate.pushInt(7)
    vmstate.pushInt(6)
    vmstate.pushInt(5)
    vmstate.pushInt(4)
    vmstate.pushInt(3)
    vmstate.pushInt(2)
    vmstate.pushInt(1)
    vmstate.pushInt(0)
      
    vmstate.stackRoll(5, 1)
    vmstate.popInt should equal (1)
    vmstate.popInt should equal (2)
    vmstate.popInt should equal (3)
    vmstate.popInt should equal (4)
    vmstate.popInt should equal (0)
  }
  it should "do StkRoll with negative rotate" in {
    vmstate.pushInt(8)
    vmstate.pushInt(7)
    vmstate.pushInt(6)
    vmstate.pushInt(5)
    vmstate.pushInt(4)
    vmstate.pushInt(3)
    vmstate.pushInt(2)
    vmstate.pushInt(1)
    vmstate.pushInt(0)
      
    vmstate.stackRoll(5, 1)
    vmstate.stackRoll(9, -3)
    vmstate.popInt should equal (6)
    vmstate.popInt should equal (7)
    vmstate.popInt should equal (8)
    vmstate.popInt should equal (1)
    vmstate.popInt should equal (2)
    vmstate.popInt should equal (3)
    vmstate.popInt should equal (4)
    vmstate.popInt should equal (0)
    vmstate.popInt should equal (5)
  }
}

