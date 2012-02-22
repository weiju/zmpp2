/*
 * Created on 2010/04/09
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
package org.zmpp.glk.windows;

import org.zmpp.glk.*;
import java.io.File;

/**
 * Interface to be implemented by the user interface, technology-dependent.
 * Represents the screen as a whole.
 */
public interface GlkScreenUI {

    GlkDimension imageSize(int imageNum);
    void updateLayout(GlkWindow root);
    GlkWindowUI createTextBufferUI(int id, GlkUIWindow glkUiWindow);
    GlkWindowUI createTextGridUI(int id, GlkUIWindow glkUiWindow);
    GlkWindowUI createGraphicsUI(int id, GlkWindow glkWindow);
  
    void requestLineInput(int windowId);

    /*
     * A small trick that works with the current ZMPP event model: If
     * the line input was suspended (e.g. by a timer input or something)
     * calling this method let's us pick up at the previous input mark.
     * requestLineInput() would simply start at wherever the cursor is
     * at the moment, while this method picks up where it was when the
     * pending line input was requested at the first time.
     */
    void requestPreviousLineInput(int windowId);
    void requestCharInput(int windowId);
    void requestMouseInput(int windowId);
    void requestTimerInput(int millis);
    void requestHyperlinkEvent(int windowId);

    String cancelLineInput(int windowId);
  
    /*
     * Asks the user interface to have the user select a file. Returns null if
     * cancelled, otherwise the full path to the file.
     */
    File selectFileByDialog(int usage, int fmode);
}
