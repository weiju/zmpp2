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
package org.zmpp.glk.windows;

import java.util.logging.*;
import org.zmpp.glk.styles.*;
import org.zmpp.glk.io.*;

/**
 * A user interface with an UI representation which it delegates to.
 * These can be of type blank, text buffer, text grid and graphics.
 */
public abstract class GlkUIWindow extends GlkWindow {
    private static Logger logger = Logger.getLogger("glk.ui");

    private int _style = 0;
    private int _writeCount = 0;
    private StringBuilder _buffer = new StringBuilder();
    private GlkStream _echoStream;

    public GlkUIWindow(int id, int size, int rock) {
        super(id, size, rock);
    }

    public abstract StyleHints styleHints();

    public GlkStream echoStream() { return _echoStream; }
    public void setEchoStream(GlkStream stream) { _echoStream = stream; }
    public GlkStream outputStream() {
        return new GlkStream() {
            private int _id = 0;

            public int id() { return _id; }
            public void setId(int anId) { _id = anId; }
            public int rock() { return 0; }
            public void close() { }
            public int readCount() {
                throw new UnsupportedOperationException("WindowStream does not support readCount");
            }
            public int getChar() {
                throw new UnsupportedOperationException("WindowStream does not support getChar");
            }
            public int getCharUni() {
                throw new UnsupportedOperationException("WindowStream does not support getCharUni");
            }
            public int writeCount() { return _writeCount; }
            public int position() { return 0; }
            public int style() { return _style; }
            public void setStyle(int value) {
                _style = value;
                ui.setStyle(value);
            }
            public void putChar(char c) {
                ui.putChar(c);
                _writeCount++;
      
                // write to echo stream
                if (_echoStream != null) _echoStream.putChar(c);
            }
            public void putCharUni(int c) {
                ui.putCharUni(c);
                _writeCount++;

                // write to echo stream
                if (_echoStream != null) _echoStream.putCharUni(c);
            }

            public void seek(int newpos, int seekmode) {
                throw new UnsupportedOperationException("WindowStream.seek() not supported");
            }
            public void setHyperlink(int linkval) {
                ui.setHyperlink(linkval);
            }
        };
    }
}