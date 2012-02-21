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

import java.util.logging.*;
import java.util.List;
import java.util.ArrayList;


public final class GlkIOSystem {

    private static Logger logger = Logger.getLogger("glk");
    private int _nextId = 1;
    private List<GlkStream> _streams = new ArrayList<GlkStream>();
    private GlkStream _currentStream = NilStream.getInstance();

    public GlkStream streamWithId(int id) {
        for (GlkStream stream: _streams) {
            if (stream.id() == id) return stream;
        }
        return null;
    }

    public int registerStream(GlkStream stream) {
        stream.setId(_nextId);
        _streams.add(stream);
        _nextId += 1;
        return stream.id();
    }

    public GlkStream iterate(int id) {
        if (_streams.isEmpty()) return null;
        else if (id == 0) return _streams.get(0);
        else {
            int i = 0;
            for (i = 0; i < _streams.size(); i++) {
                if (_streams.get(i).id() == id) break;
            }
            if (i >= _streams.size() - 1) return null;
            else return _streams.get(i + 1);
        }
    }

    public GlkStreamCloseStruct closeStream(int id) {
        GlkStream stream = streamWithId(id);
        _streams.remove(stream);
        stream.close();
        if (stream == _currentStream) _currentStream = NilStream.getInstance();
        return new GlkStreamCloseStruct(stream.writeCount(), stream.readCount());
    }

    public int getPosition(int id) { return streamWithId(id).position(); }
    public void setPosition(int id, int pos, int seekmode) {
        streamWithId(id).seek(pos, seekmode);
    }

    public void putChar(int id, char c) { streamWithId(id).putChar(c); }
    public void putCharUni(int id, int c) { streamWithId(id).putCharUni(c); }

    // control current stream
    public GlkStream currentStream() { return _currentStream; }
    public void setCurrentStream(GlkStream s) {
        _currentStream = (s == null) ? NilStream.getInstance() : s;
    }

    public int currentStreamId() { return _currentStream.id(); }
    public void setCurrentStreamId(int id) { _currentStream = streamWithId(id); }

    public int getRock(int streamId) { return streamWithId(streamId).rock(); }

    public void putChar(char c) { _currentStream.putChar(c); }
    public void putCharUni(int c) { _currentStream.putCharUni(c); }

    // For convenient output (debugging etc.)
    public void putJavaString(String str) {
        for (int i = 0; i < str.length(); i++) {
            putCharUni(str.charAt(i));
        }
    }

    public void setHyperlink(int linkval) { _currentStream.setHyperlink(linkval); }
    public void setHyperlinkStream(int streamId, int linkval) {
        streamWithId(streamId).setHyperlink(linkval);
    }

    public int currentStyle() { return _currentStream.style(); }
    public void setCurrentStyle(int value) { _currentStream.setStyle(value); }
  
    public void setStyle(int streamId, int value) {
        streamWithId(streamId).setStyle(value);
    }

    // reading
    public int getCharStream(int streamId) { return streamWithId(streamId).getChar(); }
    public int getCharStreamUni(int streamId) { return streamWithId(streamId).getCharUni(); }
}
