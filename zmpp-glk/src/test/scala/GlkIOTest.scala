/*
 * Created on 2010/04/22
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
package org.zmpp.glk

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterEach
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._
import org.zmpp.glk.io.{GlkStream, GlkIOSystem}

class DummyStream extends GlkStream {
  private[this] var _id = 0
  def id = _id
  def setId(anId: Int) { _id = anId }
  val rock = 0
  def position = 0
  
  def seek(newpos: Int, seekmode: Int) {}
  def close {}
  
  // input methods
  def readCount: Int = 0
  def getChar = {
    throw new UnsupportedOperationException("DummyStream does not support getChar")
  }
  def getCharUni = {
    throw new UnsupportedOperationException("DummyStream does not support getCharUni")
  }

  // output methods
  def writeCount: Int = 0
  def style: Int = 0
  def setStyle(value: Int) {}
  def putChar(c: Char) {}
  def putCharUni(c: Int) {}
  def setHyperlink(linkval: Int) { }
}

@RunWith(classOf[JUnitRunner])
class GlkIOSpec extends FlatSpec with ShouldMatchers {
  "GlkIOSystem" should "be initialized" in {
    val ioSystem = new GlkIOSystem
    ioSystem.currentStream.id should be (0)
    ioSystem.currentStreamId  should be (0)
    ioSystem.iterate(0) should be (null)
  }
  it should "set current stream" in {
    val ioSystem = new GlkIOSystem
    val stream = new DummyStream
    ioSystem.setCurrentStream(stream)

    ioSystem.currentStream should equal (stream)
  }
  it should "register a stream" in {
    val ioSystem = new GlkIOSystem
    val stream = new DummyStream
    ioSystem.registerStream(stream)

    ioSystem.currentStream      should not equal (stream)
    ioSystem.iterate(0)         should equal (stream)
    ioSystem.iterate(stream.id) should be (null)
  }
  it should "register a stream and close it" in {
    val ioSystem = new GlkIOSystem
    val stream = new DummyStream
    ioSystem.registerStream(stream)
    ioSystem.closeStream(stream.id)

    ioSystem.iterate(0) should be (null)
  }
  it should "register two streams" in {
    val ioSystem = new GlkIOSystem
    val stream1 = new DummyStream
    val stream2 = new DummyStream
    ioSystem.registerStream(stream1)
    ioSystem.registerStream(stream2)

    ioSystem.iterate(0)          should equal (stream1)
    ioSystem.iterate(stream1.id) should equal (stream2)
    ioSystem.iterate(stream2.id) should be (null)
  }
}
