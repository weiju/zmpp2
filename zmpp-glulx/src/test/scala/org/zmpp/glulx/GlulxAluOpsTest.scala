/*
 * Created on 2012/02/20
 * Copyright (c) 2010-2012, Wei-ju Wu.
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

/**
 * A test specification for the arithmetic and logical operations.
 */
@RunWith(classOf[JUnitRunner])
class GlulxAluOpsSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  // Sets up a getstringtbl instruction
  val DummyMem = Array[Byte](0x47, 0x6c, 0x75, 0x6c,
                             0x00, 0x03, 0x01, 0x01, // Version
                             0x00, 0x00, 0x00, 0x24, // RAMSTART
                             0x00, 0x00, 0x00, 0x38, // EXTSTART
                             0x00, 0x00, 0x00, 0x38, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x24, // STARTFUNC
                             0x04, 0x07, 0x01, 0x01, // Decoding table
                             0x01, 0x02, 0x03, 0x04, // Checksum
                             // 3 locals of size byte
                             0xc0.asInstanceOf[Byte], 0x01, 0x03, 0x00, // 0x24
                             0x00, 0x00, 0x00, 0x00, // 0x28
                             0x00, 0x00, 0x00, 0x00, // 0x2c
                             0x00, 0x00, 0x00, 0x00, // 0x30
                             0x00, 0x00, 0x00, 0x00  // 0x34
                            )
  var vm = new GlulxVM()

  override def beforeEach {
    // clear the clearable area
    for (i <- 0x28 until 0x38) DummyMem(i) = 0
  }

  "GlulxVM" should "perform an add" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x10 // add
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 0x05
    DummyMem(0x2d) = 0xff.asInstanceOf[Byte]

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (4)
  }

  it should "perform an sub" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x11 // sub
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 15
    DummyMem(0x2d) = 3

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (12)
  }

  it should "perform mul" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x12
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 15
    DummyMem(0x2d) = -3

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (-45)
  }

  it should "perform div" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x13
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 13
    DummyMem(0x2d) = 4

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (3)
  }

  it should "perform mod" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x14
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 13
    DummyMem(0x2d) = 4

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (1)
  }

  it should "perform neg" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x15
    DummyMem(0x2a) = 0x81.asInstanceOf[Byte] // address mode 1 + address mode 8

    DummyMem(0x2b) = 4

    DummyMem(0x2c) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2d) = 0x20
    vm.executeTurn
    vm.popInt should be (-4)
  }

  it should "perform bitand" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x18
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 0xff.asInstanceOf[Byte]
    DummyMem(0x2d) = 0x23

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (0xffffffff & 0x23)
  }

  it should "perform bitor" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x19
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 0xff.asInstanceOf[Byte]
    DummyMem(0x2d) = 0x23

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (0xffffffff | 0x23)
  }

  it should "perform bitxor" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x1a
    DummyMem(0x2a) = 0x11 // address mode 1 (const byte) * 2
    DummyMem(0x2b) = 0x08 // address mode 8 (stack)

    DummyMem(0x2c) = 0x03
    DummyMem(0x2d) = 0x01

    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20
    vm.executeTurn
    vm.popInt should be (0x03 ^ 0x01)
  }

  it should "perform bitnot" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x1b
    DummyMem(0x2a) = 0x81.asInstanceOf[Byte] // address mode 1 + address mode 8

    DummyMem(0x2b) = 0x0f.asInstanceOf[Byte]

    DummyMem(0x2c) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2d) = 0x20
    vm.executeTurn
    vm.popInt should be (~0x0f)
  }
}
