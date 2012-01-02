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

import java.io._

@RunWith(classOf[JUnitRunner])
class StackSpec extends FlatSpec with ShouldMatchers {

  "Stack" should "be initialized" in {
    val stack = new Stack(10)
    stack.empty should be (true)
  }
  it should "push and pop a byte" in {
    val stack = new Stack(10)
    stack.pushByte(1)
    stack.empty   should be (false)
    stack.topByte should be (1)
    stack.empty   should be (false)
    stack.popByte should be (1)
    stack.empty   should be (true)

    stack.pushByte(255)
    stack.topByte should be (255)
    stack.popByte should be (255)
  }
  it should "push and pop short" in {
    val stack = new Stack(10)
    stack.pushShort(32767)
    stack.topShort should be (32767)
    stack.pushShort(65535)
    stack.topShort should be (65535)
    stack.popShort should be (65535)
    stack.popShort should be (32767)
    stack.empty    should be (true)
  }
  it should "push and pop int" in {
    val stack = new Stack(10)
    stack.pushInt(32767)
    stack.topInt should equal (32767)
    stack.pushInt(-42)
    stack.topInt should equal (-42)
    stack.popInt should equal (-42)
    stack.popInt should equal (32767)
    stack.empty  should be (true)
  }
  it should "set and get a byte" in {
    val stack = new Stack(10)
    stack.setByte(3, 0xba)
    stack.getByte(3) should be (0xba)
    stack.sp should be (0)
  }
  it should "set and get a short" in {
    val stack = new Stack(10)
    stack.setShort(4, 0xcafe)
    stack.getShort(4) should be (0xcafe)
    stack.sp should be (0)
  }
  it should "set and get a int" in {
    val stack = new Stack(10)
    stack.setInt(4, 0xdeadbeef)
    stack.getInt(4) should be (0xdeadbeef)
    stack.sp should be (0)
  }
}

