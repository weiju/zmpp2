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
package org.zmpp.glulx

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

import java.io._
import org.zmpp.base._

@RunWith(classOf[JUnitRunner])
class StackSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val DummyMem = Array[Byte](0x47, 0x6c, 0x75, 0x6c, 0x00, 0x03, 0x01, 0x01,
                             0x00, 0x00, 0x00, 0x00, // RAMSTART
                             0x00, 0x00, 0x00, 0x00, // EXTSTART
                             0x00, 0x00, 0x00, 0x00, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x00, // STARTFUNC
                             0x00, 0x00, 0x00, 0x00, // Decoding table
                             0x00, 0x00, 0x00, 0x00 // Checksum
                            )

  var story: Memory = null
  var vmstate = new GlulxVMState

  override def beforeEach {
    story = new DefaultMemory(DummyMem)
    vmstate.init(story)
  }

  "GlulxVM stack" should "be initialized" in {
    vmstate.stackEmpty should be (true)
  }
  it should "push and pop a byte" in {
    vmstate.pushInt(0) // artificial frame len
    vmstate.pushByte(1)
    vmstate.sp should be (5)
    vmstate.topByte should be (1)
    vmstate.popByte should be (1)
    vmstate.sp should be (4)

    vmstate.pushByte(255)
    vmstate.topByte should be (255)
    vmstate.popByte should be (255)
  }
  it should "push and pop short" in {
    vmstate.pushInt(0) // artificial frame len

    vmstate.pushShort(32767)
    vmstate.topShort should be (32767)
    vmstate.pushShort(65535)

    vmstate.topShort should be (65535)
    vmstate.popShort should be (65535)
    vmstate.popShort should be (32767)
    vmstate.sp should be (4)
  }
  it should "push and pop int" in {
    vmstate.pushInt(0) // artificial frame len

    vmstate.pushInt(32767)
    vmstate.topInt should equal (32767)
    vmstate.pushInt(-42)
    vmstate.topInt should equal (-42)
    vmstate.popInt should equal (-42)
    vmstate.popInt should equal (32767)

    vmstate.sp should be (4)
  }
  it should "set and get a byte" in {
    vmstate.setByteInStack(3, 0xba)
    vmstate.getByteInStack(3) should be (0xba)
    vmstate.sp should be (0)
  }
  it should "set and get a short" in {
    vmstate.setShortInStack(4, 0xcafe)
    vmstate.getShortInStack(4) should be (0xcafe)
    vmstate.sp should be (0)
  }
  it should "set and get a int" in {
    vmstate.setIntInStack(4, 0xdeadbeef)
    vmstate.getIntInStack(4) should be (0xdeadbeef)
    vmstate.sp should be (0)
  }
}

