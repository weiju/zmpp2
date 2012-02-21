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

import org.zmpp.base.VMState;

public final class ValueEvent implements GlkEvent {

    private int windowId;
    private int eventType;
    private boolean isInternal;

    public int value1;
    public int value2;

    public ValueEvent(int eventType, int windowId,
                      int value1, int value2,
                      boolean isInternal) {
        this.eventType  = eventType;
        this.windowId   = windowId;
        this.value1     = value1;
        this.value2     = value2;
        this.isInternal = isInternal;
    }

    public ValueEvent(int eventType, int windowId, int value1,
                      int value2) {
        this(eventType, windowId, value1, value2, false);
    }

    public void process(EventManager eventManager, VMState state) {
        eventManager.removeInputRequestInWindow(windowId, eventType);
        eventManager.setEventAndResume(eventType, windowId, value1, value2);
    }
    public boolean isInternal() { return isInternal; }
    public int windowId() { return windowId; }
    public int eventType() { return eventType; }
}
