/*
 * Created on 2011/10/05
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
package org.zmpp.zcode

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach
import org.zmpp.base.Memory

@RunWith(classOf[JUnitRunner])
class EncoderSpec extends FlatSpec with ShouldMatchers {

  "Encoder" should "encode a token of ZSCII characters" in {
    val zscii = "hanky"
    val refResult = Array[Byte](0x34, 0xd3.asInstanceOf[Byte], 0x43,
                                0xc5.asInstanceOf[Byte],
                                0x94.asInstanceOf[Byte],
                                0xa5.asInstanceOf[Byte])
    val vmState = new MockVMState(5, zscii.getBytes)
    val encoder = new Encoder(vmState)
    val tokenBytes = encoder.encode(new Token(0, zscii.length - 1))
    for (i <- 0 until tokenBytes.length) {
      tokenBytes(i) should be (refResult(i))
    }
  }
  it should "encode another token of ZSCII characters" in {
    val refResult = Array[Byte](0x48,
                                0xce.asInstanceOf[Byte],
                                0x44,
                                0xf4.asInstanceOf[Byte],
                                0xf4.asInstanceOf[Byte],
                                0xa5.asInstanceOf[Byte])
    val zscii = " mailbox"
    val vmState = new MockVMState(5, zscii.getBytes)
    val encoder = new Encoder(vmState)
    val tokenBytes = encoder.encode(new Token(1, 7))
    for (i <- 0 until tokenBytes.length) {
      tokenBytes(i) should be (refResult(i))
    }
  }
  it should "encode two tokens of ZSCII characters in a row" in {
    val refResult1 = Array[Byte](0x34,
                                0xd3.asInstanceOf[Byte],
                                0x43,
                                0xc5.asInstanceOf[Byte],
                                0x94.asInstanceOf[Byte],
                                0xa5.asInstanceOf[Byte])
    val refResult2 = Array[Byte](0x48,
                                 0xce.asInstanceOf[Byte],
                                 0x44,
                                 0xf4.asInstanceOf[Byte],
                                 0xf4.asInstanceOf[Byte],
                                 0xa5.asInstanceOf[Byte])
    val zscii = "hanky mailbox"
    val vmState = new MockVMState(5, zscii.getBytes)
    val encoder = new Encoder(vmState)

    val tokenBytes1 = encoder.encode(new Token(0, 4))
    for (i <- 0 until 6) {
      tokenBytes1(i) should be (refResult1(i))
    }
    val tokenBytes2 = encoder.encode(new Token(6, 12))
    for (i <- 0 until 6) {
      tokenBytes2(i) should be (refResult2(i))
    }
  }
}
