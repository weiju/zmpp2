/*
 * Created on 2010/04/12
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
package org.zmpp.glk.events

// Note: Only Scala 2.8 has this, now we can easily iterate over Java
// collections as if they were Scala collections
import scala.collection.JavaConversions._

import java.util.LinkedList
import java.util.logging._
import scala.collection.mutable.Map
import org.zmpp.base.VMState
import org.zmpp.base.VMRunStates
import org.zmpp.glk._



// *************************************************************************
// ***** Event Manager
// ***************************
class EventManager(_state: VMState) {
  val logger = Logger.getLogger("glk")
  private var _eventPtr: Int = 0
  private var _windowEventRequests = Map[Int, List[WindowEventRequest]]()
  private var _screenUI: GlkScreenUI = null
  private var eventQueue = new LinkedList[GlkEvent]

  private def addWindowEventRequest(request: WindowEventRequest) {
    if (!(_windowEventRequests contains request.winId)) {
      _windowEventRequests(request.winId) = Nil
    }
    val requestList = _windowEventRequests(request.winId)
    if (!requestList.contains(request)) {
      _windowEventRequests(request.winId) = request :: requestList
    } else {
      logger.warning("Event request[%s] already exists for window %d !!"
                     .format(request.eventType.toString, request.winId))
    }
  }
  private def isCharInputRequestInQueueFor(winId: Int) = {
    val winReqs = eventRequestsForWindow(winId)
    !(winReqs.filter((elem) => elem.isInstanceOf[CharInputRequest]).isEmpty)
  }
  private def isLineInputRequestInQueueFor(winId: Int) = {
    val winReqs = eventRequestsForWindow(winId)
    !(winReqs.filter((elem) => elem.isInstanceOf[CharInputRequest]).isEmpty)
  }

  def setEventStruct(eventPtr: Int,
                     eventType: Int,
                     winId: Int,
                     val1: Int, val2: Int) {
    if (eventPtr == -1) {
      // On stack
      _state.pushInt(eventType)
      _state.pushInt(winId)
      _state.pushInt(val1)
      _state.pushInt(val2)
    } else if (eventPtr > 0) {
      _state.setMemIntAt(eventPtr,      eventType)
      _state.setMemIntAt(eventPtr + 4,  winId)
      _state.setMemIntAt(eventPtr + 8,  val1)
      _state.setMemIntAt(eventPtr + 12, val2)
    }
  }
  private def eventRequestsForWindow(winId: Int): List[WindowEventRequest] = {
    if (!_windowEventRequests.contains(winId)) Nil
    else _windowEventRequests(winId)
  }

  // removing event requests
  /*
  def removeLineInputRequestsInAllWindows {
    for (winId <- _windowEventRequests.keys) {
      removeLineInputRequestInWindow(winId)
    }
  }
  def removeCharInputRequestsInAllWindows {
    for (winId <- _windowEventRequests.keys) {
      removeCharInputRequestInWindow(winId)
    }
  }
  def removeMouseInputRequestsInAllWindows {
    for (winId <- _windowEventRequests.keys) {
      removeMouseInputRequestInWindow(winId)
    }
  }*/

  def removeLineInputRequestInWindow(winId: Int) {
    //logger.info("removeLineInputRequestInWindow(w: %d)".format(winId))
    val reqs = eventRequestsForWindow(winId)
    _windowEventRequests(winId) =
      reqs.filterNot(req => req.eventType == GlkEventType.LineInput)
  }
  def removeInputRequestInWindow(winId: Int, eventType: Int) {
    val reqs = eventRequestsForWindow(winId)
    _windowEventRequests(winId) =
      reqs.filterNot(req => req.eventType == eventType)
  }


  // *************************************************************************
  // * Public interface
  // *************************************************************************

  def screenUI = _screenUI
  def screenUI_=(ui: GlkScreenUI) { _screenUI = ui }

  def addCharInputRequest(winId: Int, useUnicode: Boolean) {
    if (isLineInputRequestInQueueFor(winId)) {
      removeLineInputRequestInWindow(winId)
      logger.warning("There was already a line input request for window: %d" +
                     " - replaced with char input request".format(winId))
    }
    if (isCharInputRequestInQueueFor(winId)) {
      removeInputRequestInWindow(winId, GlkEventType.CharInput)
      logger.warning("There was already a char input request for window: %d" +
                     " - replaced with char input request".format(winId))
    }
    addWindowEventRequest(new CharInputRequest(winId, useUnicode))
  }
  def addHyperlinkEventRequest(winId: Int) {
    addWindowEventRequest(new HyperlinkEventRequest(winId))
  }
  def addLineInputRequest(winId: Int, buf: Int, maxlen: Int, initlen: Int,
                          useUnicode: Boolean) {
    if (isCharInputRequestInQueueFor(winId)) {
      removeInputRequestInWindow(winId, GlkEventType.CharInput)
      logger.warning("There was already a char input request for window: %d" +
                     " - replaced with line input request".format(winId))
    }
    if (isLineInputRequestInQueueFor(winId)) {
      removeLineInputRequestInWindow(winId)
      logger.warning("There was already a line input request for window: %d" +
                     " - replaced with line input request".format(winId))
    }
    addWindowEventRequest(new LineInputRequest(winId, buf, maxlen, initlen,
                                               useUnicode))
  }
  def addMouseInputRequest(winId: Int) {
    addWindowEventRequest(new MouseInputRequest(winId))
  }
  
  def select(eventPtr: Int) {
    if (eventPtr == 0)
      throw new IllegalArgumentException(
        "eventPtr can not be null in glk_select()")
    _eventPtr = eventPtr
    _windowEventRequests.foreach(pair => {

      pair._2.foreach(req => {
        req.prepareWindow(screenUI)
      })
    })
    _state.setRunState(VMRunStates.WaitForEvent)
  }

  def selectPoll(eventPtr: Int) {
    if (eventPtr == 0)
      throw new IllegalArgumentException("eventPtr can not be null in glk_select_poll()")
    // search for
    // - timer
    // - sound notify
    // - arrange
    val event = pollInternal
    if (event != null) event.process(this, _state)
  }

  def processNextEvent: Boolean = {
    val event = this.poll
    if (event != null) {
      event.process(this, _state)
      true
    } else false
  }
  
  def setEventAndResume(eventType: Int, windowId: Int,
                        value1: Int, value2: Int) {
    setEventStruct(_eventPtr, eventType, windowId, value1, value2)    
    _state.setRunState(VMRunStates.Running)
  }

  def lineRequestForWindow(winId: Int) = {
    val eventRequests = eventRequestsForWindow(winId)
    if (eventRequests == null) null
    else {
      val lineRequests = eventRequestsForWindow(winId).filter((elem) =>
        elem.isInstanceOf[LineInputRequest])
      if (lineRequests.isEmpty) null
      else lineRequests.head.asInstanceOf[LineInputRequest]
    }
  }
  def charRequestForWindow(winId: Int) =
    eventRequestsForWindow(winId).filter((elem) =>
      elem.isInstanceOf[CharInputRequest]).head.asInstanceOf[CharInputRequest]

  // This is almost like resumeWithLineInput(), but has no effect on VM state
  // and "pulls" the incomplete input from the input line
  def cancelLineEvent(winId: Int, eventPtr: Int) {
    val lineRequest = lineRequestForWindow(winId)
    if (lineRequest != null) {
      val incompleteInput = _screenUI.cancelLineInput(winId)
      val inputLength = if (incompleteInput == null) 0
                        else incompleteInput.length
      val buffer = lineRequest.buffer
      var i = 0
      while (i < inputLength) {
        _state.setMemByteAt(buffer + i, incompleteInput.charAt(i))
        i += 1
      }
      setEventStruct(eventPtr, GlkEventType.LineInput, winId, inputLength, 0)
    }
    removeLineInputRequestInWindow(winId)
  }
  
  def requestTimerEvents(millis: Int) {
    screenUI.requestTimerInput(millis)
  }

  // ***********************************************************************
  // ***** Event Queue methods
  // *************************************
  def clear = eventQueue.synchronized { eventQueue.clear }
  private def poll: GlkEvent = eventQueue.synchronized { eventQueue.poll }
  
  private def pollInternal: GlkEvent = {
    val event = findInternal
    // I do it this way because Scala has no break and I don't want to confuse
    // the iteration
    if (event != null) eventQueue.remove(event)
    event
  }
  // This is to substitute the break, we need to exit early
  private def findInternal: GlkEvent = {
    for (event <- eventQueue) {
      if (event.isInternal) return event
    }
    null
  }

  def addTimerEvent {
    eventQueue.synchronized {
      if (!containsTimerEvent) {
        eventQueue.add(new ValueEvent(GlkEventType.Timer, 0, 0, 0, true))
      }
    }
  }
  
  def addArrangeEvent {
    eventQueue.synchronized {
      if (!containsArrangeEvent) {
        eventQueue.add(new ValueEvent(GlkEventType.Arrange, 0, 0, 0, true))
      }
    }
  }
  
  def addCharInputEvent(winId: Int, charInputCode: Int) {
    eventQueue.synchronized {
      if (!containsKeyboardInputEvent(winId)) {
        eventQueue.add(new CharInputEvent(winId, charInputCode))
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
  def addMouseEvent(winId: Int, xpos: Int, ypos: Int) {
    eventQueue.synchronized {
      eventQueue.add(new ValueEvent(GlkEventType.MouseInput, winId, xpos, ypos))
    }
  }
  
  def addSoundNotifyEvent(soundnum: Int, notifyValue: Int) {
    eventQueue.synchronized {
      eventQueue.add(new ValueEvent(GlkEventType.SoundNotify, 0, soundnum,
                                    notifyValue))
    }
  }
  
  def addHyperlinkEvent(winId: Int, hyperlinkId: Int) {
    eventQueue.synchronized {
      eventQueue.add(new ValueEvent(GlkEventType.Hyperlink, winId, hyperlinkId,
                                    0))
    }
  }

  def length  = eventQueue.synchronized { eventQueue.size }
  def isEmpty = eventQueue.synchronized { eventQueue.isEmpty }

  private def containsTimerEvent: Boolean =
    containsEventOfType(GlkEventType.Timer, 0)
  private def containsArrangeEvent: Boolean =
    containsEventOfType(GlkEventType.Arrange, 0)
  private def containsKeyboardInputEvent(winId: Int): Boolean = {
    containsEventOfType(GlkEventType.CharInput, winId) ||
    containsEventOfType(GlkEventType.LineInput, winId)
  }

  private def containsEventOfType(eventType: Int,
                                  winId: Int): Boolean = {
    for (event <- eventQueue) {
      if (event.eventType == eventType && event.windowId == winId)
        return true
    }
    false
  }
}
