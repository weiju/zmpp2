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
import org.zmpp.base.VMState;

/**
 * Byte-based memory stream
 */
public abstract class MemoryStream implements GlkStream {

    private static Logger logger = Logger.getLogger("glk");
    public VMState state;
    public int address;
    public int size;
    public int fmode;
    private int rock;
    private int _id;
    private int _style;
    private int writeCount;
    private int readCount;
    private int position;

    public MemoryStream(VMState state, int address, int size, int fmode,
                        int rock) {
        //logger.info("CREATE MEMORYSTREAM, fmode: %02x size: %d".format(fmode, size));
        this.state   = state;
        this.address = address;
        this.size    = size;
        this.fmode   = fmode;
        this.rock    = rock;

        // append mode
        if (fmode == FileModes.WriteAppend) seekToEnd();
    }

    public int id() { return _id; }
    public int rock() { return rock; }
    public void setId(int anId) { _id = anId; }
    public int style() { return _style; }
    public void setStyle(int aStyle) { _style = aStyle; }
    public int writeCount() { return writeCount; }
    public int readCount() { return readCount; }
    public int position() { return position; }

  
    private boolean positionExceedsSize() { return position >= size; }
    protected abstract int indexToPos(int index);
    protected abstract void setCurrentChar(int value);
    protected abstract int currentChar();

    private void seekToEnd() {
        position = 0;
        boolean endFound = false;
        while (!endFound && position < size) {
            if (currentChar() == 0) endFound = true;
            else position++;
        }
    }

    public void close() {
        //logger.info("Closing memory stream, WRITE COUNT = %d, READ COUNT = %d".format(
        //            writeCount, readCount))
    }
  
    public void putCharGeneric(int c) {
        if (address != 0 && size > 0 && !positionExceedsSize()) {
            setCurrentChar(c);
            position++;
        }
        writeCount++;
    }

    public void putChar(char c) { putCharGeneric((int) c); }
    public void putCharUni(int c) { putCharGeneric(c); }

    public int getCharGeneric() {
        if (positionExceedsSize()) return -1;
        else {
            readCount++;
            int retval = currentChar();
            position++;
            return retval;
        }
    }

    public int getChar() {
        int retval = getCharGeneric();
        if (retval > 255) return 0x3f;
        else return retval;
    }

    public int getCharUni() { return getCharGeneric(); }

    public void seek(int newpos, int seekmode) {
        switch (seekmode) {
        case SeekModes.Start:   position = newpos; break;
        case SeekModes.Current: position += newpos; break;
        case SeekModes.End:
            seekToEnd();
            position += newpos;
            break;
        default:
            throw new IllegalArgumentException(String.format("Unknown file seek mode: %d",
                                                             seekmode));
        }
    }
    public void setHyperlink(int linkval) {
        throw new UnsupportedOperationException("setHyperlink not supported on memory stream");
    }
}
