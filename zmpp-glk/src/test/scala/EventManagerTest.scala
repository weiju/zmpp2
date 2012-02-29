/*
 * Created on 2010/06/24
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
package org.zmpp.glk.events

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterEach
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.zmpp.base._

@RunWith(classOf[JUnitRunner])
class EventQueueSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {
  val vmState = new VMState {
    def runState = 0;
    def setRunState(state: Int) { }

    // memory access
    def memByteAt(address: Int) = 0
    def setMemByteAt(address: Int, value: Int) { }
    def memShortAt(address: Int) = 0
    def setMemShortAt(address: Int, value: Int) { }
    def memIntAt(address: Int) = 0
    def setMemIntAt(address: Int, value: Int) { }

    // stack access
    def pushByte(value: Int) { }
    def topByte = 0
    def popByte = 0
    def pushShort(value: Int) { }
    def topShort = 0
    def popShort = 0
    def pushInt(value: Int) { }
    def topInt = 0
    def popInt = 0
  }

  var eventQueue: EventManager = null

  override def beforeEach {
    eventQueue = new EventManager(vmState)
  }
  "EventManager" should "be initialized" in {
    eventQueue.length should be (0)
    eventQueue.isEmpty should be (true)
  }
  it should "add only one timer event" in {
    eventQueue.addTimerEvent
    eventQueue.addTimerEvent
    eventQueue.length should be (1)
    eventQueue.isEmpty should be (false)
  }
  it should "add only one arrange event" in {
    eventQueue.addArrangeEvent
    eventQueue.addArrangeEvent
    eventQueue.length should be (1)
  }
  it should "add only one keyboard event for same window" in {
    eventQueue.addCharInputEvent(1, 10)
    eventQueue.addCharInputEvent(1, 10)
    eventQueue.addLineInputEvent(1, "Hallo")
    eventQueue.length should be (1)
  }
  it should "add two char events for different windows" in {
    eventQueue.addCharInputEvent(1, 10)
    eventQueue.addCharInputEvent(2, 10)
    eventQueue.length should be (2)
  }
  it should "add only one line event for same window" in {
    eventQueue.addLineInputEvent(1, "Hallo")
    eventQueue.addLineInputEvent(1, "Hallo2")
    eventQueue.addCharInputEvent(1, 10)
    eventQueue.length should be (1)
  }
  it should "add two line events for different windows" in {
    eventQueue.addLineInputEvent(1, "Hallo")
    eventQueue.addLineInputEvent(2, "Hallo")
    eventQueue.length should be (2)
  }
}
