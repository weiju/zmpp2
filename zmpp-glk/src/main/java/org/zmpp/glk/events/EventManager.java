/*
 * Created on 2012/02/21
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
package org.zmpp.glk.events;

import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.logging.*;
import org.zmpp.base.VMState;
import org.zmpp.base.VMRunStates;
import org.zmpp.glk.*;
import org.zmpp.glk.windows.*;


public final class EventManager {
    private static Logger logger = Logger.getLogger("glk");
    private int _eventPtr;
    private Map<Integer, List<WindowEventRequest> > _windowEventRequests =
        new HashMap<Integer, List<WindowEventRequest> >();

    public GlkScreenUI screenUI;
    private LinkedList<GlkEvent> eventQueue = new LinkedList<GlkEvent>();
    private VMState _state;

    public EventManager(VMState state) {
        this._state = state;
    }

    private void addWindowEventRequest(WindowEventRequest request) {
        if (!(_windowEventRequests.containsKey(request.winId))) {
            _windowEventRequests.put(request.winId, new ArrayList<WindowEventRequest>());
        }
        List<WindowEventRequest> requestList = _windowEventRequests.get(request.winId);
        if (!requestList.contains(request)) {
            requestList.add(request);
        } else {
            logger.warning(String.format("Event request[%d] already exists for window %d !!",
                                         request.eventType, request.winId));
        }
    }

    private boolean isCharInputRequestInQueueFor(int winId) {
        List<WindowEventRequest> winReqs = eventRequestsForWindow(winId);
        for (WindowEventRequest req: winReqs) {
            if (req instanceof CharInputRequest) return true;
        }
        return false;
    }

    private boolean isLineInputRequestInQueueFor(int winId) {
        List<WindowEventRequest> winReqs = eventRequestsForWindow(winId);
        for (WindowEventRequest req: winReqs) {
            if (req instanceof LineInputRequest) return true;
        }
        return false;
    }

    public void setEventStruct(int eventPtr, int eventType, int winId,
                               int val1, int val2) {
        if (eventPtr == -1) {
            // On stack
            _state.pushInt(eventType);
            _state.pushInt(winId);
            _state.pushInt(val1);
            _state.pushInt(val2);
        } else if (eventPtr > 0) {
            _state.setMemIntAt(eventPtr,      eventType);
            _state.setMemIntAt(eventPtr + 4,  winId);
            _state.setMemIntAt(eventPtr + 8,  val1);
            _state.setMemIntAt(eventPtr + 12, val2);
        }
    }

    private List<WindowEventRequest> eventRequestsForWindow(int winId) {
        if (!_windowEventRequests.containsKey(winId)) return new LinkedList<WindowEventRequest>();
        else return _windowEventRequests.get(winId);
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

    public void removeLineInputRequestInWindow(int winId) {
        removeInputRequestInWindow(winId, GlkEventType.LineInput);
    }

    public void removeInputRequestInWindow(int winId, int eventType) {
        Iterator<WindowEventRequest> reqIterator = eventRequestsForWindow(winId).iterator();
        while (reqIterator.hasNext()) {
            WindowEventRequest req = reqIterator.next();
            if (req.eventType == eventType) reqIterator.remove();
        }
    }

    // *************************************************************************
    // * Public interface
    // *************************************************************************

    public void addCharInputRequest(int winId, boolean useUnicode) {
        if (isLineInputRequestInQueueFor(winId)) {
            removeLineInputRequestInWindow(winId);
            logger.warning(String.format("There was already a line input request for window: %d" +
                                         " - replaced with char input request", winId));
        }
        if (isCharInputRequestInQueueFor(winId)) {
            removeInputRequestInWindow(winId, GlkEventType.CharInput);
            logger.warning(String.format("There was already a char input request for window: %d" +
                                         " - replaced with char input request", winId));
        }
        addWindowEventRequest(new CharInputRequest(winId, useUnicode));
    }

    public void addHyperlinkEventRequest(int winId) {
        addWindowEventRequest(new HyperlinkEventRequest(winId));
    }

    public void addLineInputRequest(int winId, int buf, int maxlen, int initlen,
                                    boolean useUnicode) {
        if (isCharInputRequestInQueueFor(winId)) {
            removeInputRequestInWindow(winId, GlkEventType.CharInput);
            logger.warning(String.format("There was already a char input request for window: %d" +
                                         " - replaced with line input request", winId));
        }
        if (isLineInputRequestInQueueFor(winId)) {
            removeLineInputRequestInWindow(winId);
            logger.warning(String.format("There was already a line input request for window: %d" +
                                         " - replaced with line input request", winId));
        }
        addWindowEventRequest(new LineInputRequest(winId, buf, maxlen, initlen,
                                                   useUnicode));
    }

    public void addMouseInputRequest(int winId) {
        addWindowEventRequest(new MouseInputRequest(winId));
    }

    public void select(int eventPtr) {
        if (eventPtr == 0)
            throw new IllegalArgumentException("eventPtr can not be null in glk_select()");
        _eventPtr = eventPtr;
        for (Map.Entry<Integer, List<WindowEventRequest>> entry : _windowEventRequests.entrySet()) {
            for (WindowEventRequest req : entry.getValue()) {
                req.prepareWindow(screenUI);
            }
        }
        _state.setRunState(VMRunStates.WaitForEvent);
    }

    public void selectPoll(int eventPtr) {
        if (eventPtr == 0)
            throw new IllegalArgumentException("eventPtr can not be null in glk_select_poll()");
        // search for
        // - timer
        // - sound notify
        // - arrange
        GlkEvent event = pollInternal();
        if (event != null) event.process(this, _state);
    }

    public boolean processNextEvent() {
        GlkEvent event = this.poll();
        if (event != null) {
            event.process(this, _state);
            return true;
        } else return false;
    }
  
    public void setEventAndResume(int eventType, int windowId,
                                  int value1, int value2) {
        setEventStruct(_eventPtr, eventType, windowId, value1, value2);
        _state.setRunState(VMRunStates.Running);
    }

    public LineInputRequest lineRequestForWindow(int winId) {
        for (WindowEventRequest req : eventRequestsForWindow(winId)) {
            if (req instanceof LineInputRequest) return (LineInputRequest) req;
        }
        return null;
    }

    public CharInputRequest charRequestForWindow(int winId) {
        for (WindowEventRequest req : eventRequestsForWindow(winId)) {
            if (req instanceof CharInputRequest) return (CharInputRequest) req;
        }
        return null;
    }

    // This is almost like resumeWithLineInput(), but has no effect on VM state
    // and "pulls" the incomplete input from the input line
    public void cancelLineEvent(int winId, int eventPtr) {
        LineInputRequest lineRequest = lineRequestForWindow(winId);
        if (lineRequest != null) {
            String incompleteInput = screenUI.cancelLineInput(winId);
            int inputLength = (incompleteInput == null) ? 0 : incompleteInput.length();
            int buffer = lineRequest.buffer;
            for (int i = 0; i < inputLength; i++) {
                _state.setMemByteAt(buffer + i, incompleteInput.charAt(i));
            }
            setEventStruct(eventPtr, GlkEventType.LineInput, winId, inputLength, 0);
        }
        removeLineInputRequestInWindow(winId);
    }
  
    public void requestTimerEvents(int millis) {
        screenUI.requestTimerInput(millis);
    }

    // ***********************************************************************
    // ***** Event Queue methods
    // *************************************
    public void clear() {
        synchronized (eventQueue) {
            eventQueue.clear();
        }
    }

    public GlkEvent poll() {
        synchronized (eventQueue) {
            return eventQueue.poll();
        }
    }
  
    private GlkEvent pollInternal() {
        GlkEvent event = findInternal();
        // I do it this way because Scala has no break and I don't want to confuse
        // the iteration
        if (event != null) eventQueue.remove(event);
        return event;
    }

    // This is to substitute the break, we need to exit early
    private GlkEvent findInternal() {
        for (GlkEvent event: eventQueue) {
            if (event.isInternal()) return event;
        }
        return null;
    }

    public void addTimerEvent() {
        synchronized (eventQueue) {
            if (!containsTimerEvent()) {
                eventQueue.add(new ValueEvent(GlkEventType.Timer, 0, 0, 0, true));
            }
        }
    }
  
    public void addArrangeEvent() {
        synchronized (eventQueue) {
            if (!containsArrangeEvent()) {
                eventQueue.add(new ValueEvent(GlkEventType.Arrange, 0, 0, 0, true));
            }
        }
    }
  
    public void addCharInputEvent(int winId, int charInputCode) {
        synchronized (eventQueue) {
            if (!containsKeyboardInputEvent(winId)) {
                eventQueue.add(new CharInputEvent(winId, charInputCode));
            }
        }
    }

    public void addLineInputEvent(int winId, String input) {
        synchronized (eventQueue) {
            if (!containsKeyboardInputEvent(winId)) {
                eventQueue.add(new LineInputEvent(winId, input));
            }
        }
    }

    public void addMouseEvent(int winId, int xpos, int ypos) {
        synchronized (eventQueue) {
            eventQueue.add(new ValueEvent(GlkEventType.MouseInput, winId, xpos, ypos));
        }
    }
  
    public void addSoundNotifyEvent(int soundnum, int notifyValue) {
        synchronized (eventQueue) {
            eventQueue.add(new ValueEvent(GlkEventType.SoundNotify, 0, soundnum,
                                          notifyValue));
        }
    }
  
    public void addHyperlinkEvent(int winId, int hyperlinkId) {
        synchronized (eventQueue) {
            eventQueue.add(new ValueEvent(GlkEventType.Hyperlink, winId, hyperlinkId, 0));
        }
    }

    public int length() {
        synchronized (eventQueue) {
            return eventQueue.size();
        }
    }

    public boolean isEmpty() {
        synchronized (eventQueue) {
            return eventQueue.isEmpty();
        }
    }

    private boolean containsTimerEvent() {
        return containsEventOfType(GlkEventType.Timer, 0);
    }

    private boolean containsArrangeEvent() {
        return containsEventOfType(GlkEventType.Arrange, 0);
    }

    private boolean containsKeyboardInputEvent(int winId) {
        return containsEventOfType(GlkEventType.CharInput, winId) ||
            containsEventOfType(GlkEventType.LineInput, winId);
    }

  private boolean containsEventOfType(int eventType, int winId) {
      for (GlkEvent event : eventQueue) {
          if (event.eventType() == eventType && event.windowId() == winId)
              return true;
      }
      return false;
  }
}
