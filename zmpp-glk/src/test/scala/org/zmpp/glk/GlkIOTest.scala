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
package org.zmpp.glk

import org.specs._
import org.specs.matcher._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class GlkIOTest extends JUnit4(GlkIOSpec)
object GlkIOSpecRunner extends ConsoleRunner(GlkIOSpec)

class DummyStream extends GlkStream {
  var id: Int = 0
  val rock = 0
  def position = 0
  
  def seek(newpos: Int, seekmode: Int) {}
  def close {}
  
  // input methods
  def readCount: Int = 0

  // output methods
  def writeCount: Int = 0
  def style: Int = 0
  def style_=(value: Int) {}
  def putChar(c: Char) {}
  def putCharUni(c: Int) {}
  def setHyperlink(linkval: Int) { }
}

/**
 * Note: We compare with xUnit matchers, there seems to be a Scala/Specs bug, which
 * tries to use String.isEmpty which only exists in Java SE 6
 */
object GlkIOSpec extends Specification with xUnit {
  "GlkIOSystem" should {
    "be initialized" in {
      val ioSystem = new GlkIOSystem
      ioSystem.currentStream.id must_== 0
      ioSystem.currentStreamId must_== 0
      assertNull(ioSystem.iterate(0))
    }
    "set current stream" in {
      val ioSystem = new GlkIOSystem
      val stream = new DummyStream
      ioSystem.currentStream = stream
      assertTrue(ioSystem.currentStream == stream)
    }
    "register a stream" in {
      val ioSystem = new GlkIOSystem
      val stream = new DummyStream
      ioSystem.registerStream(stream)
      assertTrue(ioSystem.currentStream != stream)
      assertTrue(ioSystem.iterate(0) == stream)
      assertNull(ioSystem.iterate(stream.id))
    }
    "register a stream and close it" in {
      val ioSystem = new GlkIOSystem
      val stream = new DummyStream
      ioSystem.registerStream(stream)
      ioSystem.closeStream(stream.id)
      assertTrue(ioSystem.iterate(0) == null)
    }
    "register two streams" in {
      val ioSystem = new GlkIOSystem
      val stream1 = new DummyStream
      val stream2 = new DummyStream
      ioSystem.registerStream(stream1)
      ioSystem.registerStream(stream2)
      assertTrue(ioSystem.iterate(0) == stream2)
      assertTrue(ioSystem.iterate(stream2.id) == stream1)
      assertNull(ioSystem.iterate(stream1.id))
    }
  }
}
