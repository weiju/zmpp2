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
                             0x00, 0x00, 0x00, 0x2c, // EXTSTART
                             0x00, 0x00, 0x00, 0x2c, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x24, // STARTFUNC
                             0x04, 0x07, 0x01, 0x01, // Decoding table
                             0x01, 0x02, 0x03, 0x04, // Checksum
                             0xc0.asInstanceOf[Byte], 0x00, 0x00, 0x81.asInstanceOf[Byte],  // (0x24 = 36)
                             0x20, 0x00, 0x00, 0x00  // quit instruction
                            )
  var vm = new GlulxVM()

  "GlulxVM" should "be in a defined state after initState()" in {
    vm.initState(DummyMem)
    vm.isGlulx    should be (true)
    vm.pc         should equal (0)
    vm.fp         should equal (0)
    vm.version    should equal (0x00030101)
    vm.ramstart   should equal (0x24)
    vm.extstart   should equal (0x2c)
    vm.endmem     should equal (0x2c)
    vm.stacksize  should equal (255)
    vm.startfunc  should equal (0x24)
    vm.checksum   should equal (0x01020304)
    vm.runState   should equal (VMRunStates.Running)
    vm.memsize    should equal (0x2c)
    vm.currentDecodingTable should equal (0x04070101)
    vm.sp         should equal (0)
  }

  it should "be in a defined state after init()" in {
    vm.init(DummyMem, null)
    vm.isGlulx    should be (true)
    // frame size + locals pos + localType/localCount + 2 pad bytes + numArguments
    vm.sp         should be (16)
    vm.pc         should equal (0x27)
    vm.fp         should equal (0)
  }

  it should "be in halted state after quit" in {
    vm.init(DummyMem, null)
    vm.isGlulx    should be (true)
    vm.executeTurn
    vm.runState   should be (VMRunStates.Halted)
  }
}

@RunWith(classOf[JUnitRunner])
class GlulxVMAddressModeSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  val DummyMem = Array[Byte](0x47, 0x6c, 0x75, 0x6c,
                             0x00, 0x03, 0x01, 0x01, // Version
                             0x00, 0x00, 0x00, 0x24, // RAMSTART
                             0x00, 0x00, 0x00, 0x34, // EXTSTART
                             0x00, 0x00, 0x00, 0x34, // ENDMEM
                             0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                             0x00, 0x00, 0x00, 0x24, // STARTFUNC
                             0x04, 0x07, 0x01, 0x01, // Decoding table
                             0x01, 0x02, 0x03, 0x04, // Checksum
                             0xc0.asInstanceOf[Byte], 0x00, 0x00, 0x81.asInstanceOf[Byte],  // (0x24 = 36)
                             0x41, 0x00, 0x00, 0x00,  // setstringtbl
                             0x00, 0x00, 0x00, 0x00,
                             0x00, 0x00, 0x00, 0x00
                            )
  var vm = new GlulxVM()

  override def beforeEach {
    // clear the clearable area
    for (i <- 0x29 until 0x34) DummyMem(i) = 0
  }


  "GlulxVM" should "read address mode const 0" in {
    vm.init(DummyMem, null)
    DummyMem(0x29) = 0x00 // address mode 0 (ConstZero)
    DummyMem(0x2a) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2b) = 0x20
    vm.executeTurn
    vm.currentDecodingTable should be (0)
  }

  it should "read address mode const byte" in {
    DummyMem(0x29) = 0x01 // address mode 1 (ConstByte)
    DummyMem(0x2a) = 0x42 // value
    DummyMem(0x2b) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2c) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (0x42)
  }
  it should "read address mode const byte sign extended" in {
    DummyMem(0x29) = 0x01 // address mode 1 (ConstByte)
    DummyMem(0x2a) = 0xff.asInstanceOf[Byte] // value
    DummyMem(0x2b) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2c) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (-1)
  }

  it should "read address mode const short" in {
    DummyMem(0x29) = 0x02 // address mode 2 (ConstShort)
    DummyMem(0x2a) = 0x42 // value
    DummyMem(0x2b) = 0x42
    DummyMem(0x2c) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2d) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (0x4242)
  }
  it should "read address mode const short sign extended" in {
    DummyMem(0x29) = 0x02 // address mode 2 (ConstShort)
    DummyMem(0x2a) = 0xff.asInstanceOf[Byte] // value
    DummyMem(0x2b) = 0xfe.asInstanceOf[Byte]
    DummyMem(0x2c) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2d) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (-2)
  }

  it should "read address mode const int" in {
    DummyMem(0x29) = 0x03 // address mode 3 (ConstInt)
    DummyMem(0x2a) = 0x42 // value
    DummyMem(0x2b) = 0x43
    DummyMem(0x2c) = 0x44
    DummyMem(0x2d) = 0x45
    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (0x42434445)
  }

  it should "read address mode address $00-$FF" in {
    DummyMem(0x29) = 0x05 // address mode 5
    DummyMem(0x2a) = 0x20 // value
    DummyMem(0x2b) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2c) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (0x01020304)
  }

  it should "read address mode address $0000-$FFFF" in {
    DummyMem(0x29) = 0x06 // address mode 6
    DummyMem(0x2a) = 0x00 // value
    DummyMem(0x2b) = 0x20 // value
    DummyMem(0x2c) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2d) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (0x01020304)
  }
  it should "read address mode address any" in {
    DummyMem(0x29) = 0x07 // address mode 7
    DummyMem(0x2a) = 0x00 // value
    DummyMem(0x2b) = 0x00 // value
    DummyMem(0x2c) = 0x00 // value
    DummyMem(0x2d) = 0x20 // value
    DummyMem(0x2e) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2f) = 0x20

    vm.init(DummyMem, null)
    vm.executeTurn
    vm.currentDecodingTable should be (0x01020304)
  }

  it should "read address mode stack" in {
    DummyMem(0x29) = 0x08 // address mode 8
    DummyMem(0x2a) = 0x81.asInstanceOf[Byte] // quit instruction
    DummyMem(0x2b) = 0x20

    vm.init(DummyMem, null)
    // need to push after initialization !!!
    vm.pushInt(4711)
    vm.executeTurn
    vm.currentDecodingTable should be (4711)
  }
}
