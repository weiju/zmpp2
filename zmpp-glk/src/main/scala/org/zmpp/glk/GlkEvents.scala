/*
 * Created on 2010/04/12
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

import java.util.logging._
import scala.collection.mutable.Map
import org.zmpp.base.VMState
import org.zmpp.base.VMRunStates

object GlkEventType extends Enumeration {
  val EventNone   = Value("EventNone")
  val Timer       = Value("Timer")
  val CharInput   = Value("CharInput")
  val LineInput   = Value("LineInput")
  val MouseInput  = Value("MouseInput")
  val Arrange     = Value("Arrange")
  val Redraw      = Value("Redraw")
  val Hyperlink   = Value("Hyperlink")
}

object GlkKeyCodes {
  val Unknown  = 0xffffffff
  val Left     = 0xfffffffe
  val Right    = 0xfffffffd
  val Up       = 0xfffffffc
  val Down     = 0xfffffffb
  val Return   = 0xfffffffa
  val Delete   = 0xfffffff9
  val Escape   = 0xfffffff8
  val Tab      = 0xfffffff7
  val PageUp   = 0xfffffff6
  val PageDown = 0xfffffff5
  val Home     = 0xfffffff4
  val End      = 0xfffffff3
  val Func1    = 0xffffffef
  val Func2    = 0xffffffee
  val Func3    = 0xffffffed
  val Func4    = 0xffffffec
  val Func5    = 0xffffffeb
  val Func6    = 0xffffffea
  val Func7    = 0xffffffe9
  val Func8    = 0xffffffe8
  val Func9    = 0xffffffe7
  val Func10   = 0xffffffe6
  val Func11   = 0xffffffe5
  val Func12   = 0xffffffe4
}

trait EventRequest

/**
 * Event requests that are window-dependent
 */
abstract class WindowEventRequest(val winId: Int) extends EventRequest {
  // use visitor pattern to set the window events in the native user interface
  // side
  def prepareWindow(screenUI: GlkScreenUI)
}

class LineInputRequest(winId: Int, val buffer: Int, val maxlen: Int, val initlen: Int)
extends WindowEventRequest(winId) {
  private var runOnce = false
  override def equals(that: Any): Boolean = that match {
    case other: LineInputRequest => winId == other.winId
    case _ => false
  }
  def prepareWindow(screenUI: GlkScreenUI) {
    if (runOnce) {
      screenUI.requestPreviousLineInput(winId)
    } else {
      screenUI.requestLineInput(winId)
      runOnce = true
    }
  }
  override def hashCode = winId
}

class CharInputRequest(winId: Int) extends WindowEventRequest(winId) {
  override def equals(that: Any): Boolean = that match {
    case other: CharInputRequest => winId == other.winId
    case _ => false
  }
  def prepareWindow(screenUI: GlkScreenUI) {
    screenUI.requestCharInput(winId)
  }
  override def hashCode = winId
}
class MouseInputRequest(winId: Int) extends WindowEventRequest(winId) {
  override def equals(that: Any): Boolean = that match {
    case other: MouseInputRequest => winId == other.winId
    case _ => false
  }
  def prepareWindow(screenUI: GlkScreenUI) {
    screenUI.requestMouseInput(winId)
  }
  override def hashCode = winId
}

class EventManager(_state: VMState) {
  val logger = Logger.getLogger("glk")
  private var _eventPtr: Int = 0
  private var _windowEventRequests = Map[Int, List[WindowEventRequest]]()
  private var _screenUI: GlkScreenUI = null

  //private var _otherEventRequests = Set[EventRequest]()
  private def addWindowEventRequest(request: WindowEventRequest) {
    if (!(_windowEventRequests contains request.winId)) {
      _windowEventRequests(request.winId) = Nil
    }
    val requestList = _windowEventRequests(request.winId)
    if (requestList contains request) {
      throw new IllegalStateException("EVENT REQUEST ALREADY EXISTS IN THE QUEUE FOR WINDOW %d !!".format(request.winId))
    }
    _windowEventRequests(request.winId) = request :: requestList
  }
  
  private def isKeyboardInputRequestInQueueFor(winId: Int) = {
    val winReqs = eventRequestsForWindow(winId)
    !(winReqs.filter((elem) => elem.isInstanceOf[LineInputRequest]).isEmpty) ||
    !(winReqs.filter((elem) => elem.isInstanceOf[CharInputRequest]).isEmpty)
  }

  private def lineRequestForWindow(winId: Int) =
    eventRequestsForWindow(winId).filter((elem) =>
      elem.isInstanceOf[LineInputRequest]).head.asInstanceOf[LineInputRequest]
  private def charRequestForWindow(winId: Int) =
    eventRequestsForWindow(winId).filter((elem) =>
      elem.isInstanceOf[CharInputRequest]).head.asInstanceOf[CharInputRequest]

  private def setEventStruct(eventPtr: Int, eventType: GlkEventType.Value,
                             winId: Int,
                             val1: Int, val2: Int) {
    if (eventPtr == -1) {
      // On stack
      _state.pushInt(eventType.id)
      _state.pushInt(winId)
      _state.pushInt(val1)
      _state.pushInt(val2)
    } else {
      _state.setMemIntAt(eventPtr,      eventType.id)
      _state.setMemIntAt(eventPtr + 4,  winId)
      _state.setMemIntAt(eventPtr + 8,  val1)
      _state.setMemIntAt(eventPtr + 12, val2)
    }
  }
  private def eventRequestsForWindow(winId: Int): List[WindowEventRequest] = {
    if (!_windowEventRequests.contains(winId)) Nil else _windowEventRequests(winId)
  }

  // removing event requests
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
  }

  def removeLineInputRequestInWindow(winId: Int) {
    //logger.info("removeLineInputRequestInWindow(w: %d)".format(winId))
    val reqs = eventRequestsForWindow(winId)
    _windowEventRequests(winId) = reqs.filterNot(req => req.isInstanceOf[LineInputRequest])
  }
  def removeCharInputRequestInWindow(winId: Int) {
    //logger.info("removeCharInputRequestInWindow(w: %d)".format(winId))
    val reqs = eventRequestsForWindow(winId)
    _windowEventRequests(winId) = reqs.filterNot(req => req.isInstanceOf[CharInputRequest])
  }
  def removeMouseInputRequestInWindow(winId: Int) {
    //logger.info("removeMouseInputRequestInWindow(w: %d)".format(winId))
    val reqs = eventRequestsForWindow(winId)
    _windowEventRequests(winId) = reqs.filterNot(req => req.isInstanceOf[MouseInputRequest])
  }

  /*************************************************************************
   *
   * Public interface
   *
   *************************************************************************/

  def screenUI = _screenUI
  def screenUI_=(ui: GlkScreenUI) { _screenUI = ui }

  def addCharInputRequest(winId: Int) {
    if (isKeyboardInputRequestInQueueFor(winId)) {
      throw new IllegalArgumentException("There is already an input request in the queue for window: %d".format(winId))
    }
    addWindowEventRequest(new CharInputRequest(winId))
  }
  def addLineInputRequest(winId: Int, buf: Int, maxlen: Int, initlen: Int) {
    if (isKeyboardInputRequestInQueueFor(winId)) {
      throw new IllegalArgumentException("There is already an input request in the queue for window: %d".format(winId))
    }
    addWindowEventRequest(new LineInputRequest(winId, buf, maxlen, initlen))
  }
  def addMouseInputRequest(winId: Int) {
    addWindowEventRequest(new MouseInputRequest(winId))
  }
  
  def select(eventPtr: Int) {
    if (eventPtr == 0)
      throw new IllegalArgumentException("eventPtr can not be null in glk_select()")
    _eventPtr = eventPtr
    //logger.info("glk_select(#$%02x)".format(eventPtr))
    _windowEventRequests.foreach(pair => {
      //logger.info(
      //  "window id to select: %d # events: %d".format(pair._1, pair._2.length))
      pair._2.foreach(req => {
        //logger.info("preparing request: %s".format(req.toString))
        req.prepareWindow(screenUI)
      })
    })
    _state.runState = VMRunStates.WaitForEvent
  }

  def selectPoll(eventPtr: Int) {
    if (eventPtr == 0)
      throw new IllegalArgumentException("eventPtr can not be null in glk_select_poll()")
    setEventStruct(eventPtr, screenUI.pollEvents, 0, 0, 0)
  }

  def resumeWithLineInput(winId: Int, line: String) {
    logger.info("resumeWithLineInput(w: %d, line: '%s')".format(winId, line))
    val lineRequest = lineRequestForWindow(winId)
    val buffer = lineRequest.buffer
    for (i <- 0 until line.length) {
      _state.setMemByteAt(buffer + i, line.charAt(i))
    }
    setEventStruct(_eventPtr, GlkEventType.LineInput, winId, line.length, 0)    
    removeLineInputRequestsInAllWindows
    _state.runState = VMRunStates.Running
  }

  def resumeWithCharInput(winId: Int, keyCode: Int) {
    logger.info("resumeWithCharInput(w: %d, keyCode: %d)".format(winId, keyCode))
    setEventStruct(_eventPtr, GlkEventType.CharInput, winId, keyCode, 0)
    removeCharInputRequestsInAllWindows
    _state.runState = VMRunStates.Running
  }
  
  def resumeWithMouseInput(winId: Int, xpos: Int, ypos: Int) {
    logger.info("resumeWithMouseInput(w: %d, x: %d y: %d)".format(winId, xpos, ypos))
    setEventStruct(_eventPtr, GlkEventType.MouseInput, winId, xpos, ypos)
    removeMouseInputRequestsInAllWindows
    _state.runState = VMRunStates.Running
  }
  
  def resumeWithTimerEvent {
    //logger.info("resumeWithTimerEvent")
    setEventStruct(_eventPtr, GlkEventType.Timer, 0, 0, 0)
    _state.runState = VMRunStates.Running
  }

  /*
   * This is almost like resumeWithLineInput(), but has no effect on VM state
   * and "pulls" the incomplete input from the input line
   */
  def cancelLineEvent(winId: Int, eventPtr: Int) {
    val lineRequest = lineRequestForWindow(winId)
    if (lineRequest != null) {
      val incompleteInput = _screenUI.cancelLineInput(winId)
      val inputLength = if (incompleteInput == null) 0 else incompleteInput.length
      val buffer = lineRequest.buffer
      for (i <- 0 until inputLength) {
        _state.setMemByteAt(buffer + i, incompleteInput.charAt(i))
      }
      setEventStruct(eventPtr, GlkEventType.LineInput, winId, inputLength, 0)
    }
    removeLineInputRequestInWindow(winId)
  }
  
  def requestTimerEvents(millis: Int) {
    screenUI.requestTimerInput(millis)
  }
}

