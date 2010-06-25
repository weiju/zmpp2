/*
 * Created on 2010/06/20
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
package org.zmpp.glulx.swing

// Note: Only Scala 2.8 has this, now we can easily iterate over Java
// collections as if they were Scala collections
import scala.collection.JavaConversions._

import java.util.logging._
import java.util.LinkedList
import org.zmpp.glk._

abstract class GlkEvent {
  def eventType: GlkEventType.Value
  def windowId : Int
}

class ValueEvent(val eventType: GlkEventType.Value,
                 val windowId : Int,
                 val value1   : Int,
                 val value2   : Int) extends GlkEvent

class LineInputEvent(val windowId: Int, input: String) extends GlkEvent {
  def eventType = GlkEventType.LineInput
}

    // TODO: Only enqeueue events if possible, e.g. not
    // - two line/char events in the same window
    // - two timer events
    // - two rearrange events
class GlkEventQueue {
  private var eventQueue = new LinkedList[GlkEvent]
  
  def clear = eventQueue.synchronized { eventQueue.clear }
  def poll: GlkEvent = eventQueue.synchronized { eventQueue.poll }
  def addTimerEvent {
    eventQueue.synchronized {
      if (!containsTimerEvent) {
        eventQueue.add(new ValueEvent(GlkEventType.Timer, 0, 0, 0))
      }
    }
  }
  
  def addArrangeEvent {
    eventQueue.synchronized {
      if (!containsArrangeEvent) {
        eventQueue.add(new ValueEvent(GlkEventType.Arrange, 0, 0, 0))
      }
    }
  }
  
  def addCharInputEvent(winId: Int, charInputCode: Int) {
    eventQueue.synchronized {
      if (!containsKeyboardInputEvent(winId)) {
        eventQueue.add(new ValueEvent(GlkEventType.CharInput, winId, charInputCode, 0))
      }
    }
  }
  def addLineInputEvent(winId: Int, input: String) {
    eventQueue.synchronized {
      if (!containsKeyboardInputEvent(winId)) {
        eventQueue.add(new LineInputEvent(winId, input))
      }
    }
  }

  def length  = eventQueue.synchronized { eventQueue.size }
  def isEmpty = eventQueue.synchronized { eventQueue.isEmpty }

  private def containsTimerEvent: Boolean = containsEventOfType(GlkEventType.Timer, 0)
  private def containsArrangeEvent: Boolean = containsEventOfType(GlkEventType.Arrange, 0)
  private def containsKeyboardInputEvent(winId: Int): Boolean = {
    containsEventOfType(GlkEventType.CharInput, winId) ||
    containsEventOfType(GlkEventType.LineInput, winId)
  }

  private def containsEventOfType(eventType: GlkEventType.Value, winId: Int): Boolean = {
    for (event <- eventQueue) {
      if (event.eventType == eventType && event.windowId == winId) return true
    }
    false
  }
}


