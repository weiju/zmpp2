/*
 * Created on 2012/02/20
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
package org.zmpp.glk.io;

/*
 * This stream is not actually used, we just have it to emulate Glulxe's
 * behavior of having the game file available as a stream. ZMPP does not
 * open the game through Glk streams.
 */
public final class DummyStream implements GlkStream {
    private int _id = 0;

    public int id() { return _id; }
    public void setId(int anId) { _id = anId; }
    public int rock() { return 0; }
    public int style() { return 0; }
    public void setStyle(int value) { }
    public int writeCount() { return 0; }
    public int position() { return 0; }
    public void close() {}
    public void putChar(char c) { }
    public void putCharUni(int c) { }
    public void seek(int newpos, int seekmode) { }
    public void setHyperlink(int linkval) { }
    public int readCount() { return 0; }
    public int getChar() {
        throw new UnsupportedOperationException("DummyStream does not support getChar");
    }
    public int getCharUni() {
        throw new UnsupportedOperationException("DummyStream does not support getCharUni");
    }
}
