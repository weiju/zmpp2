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
import java.io.*;

/**
 * File streams are based on RandomAccessFile, this is probably the only
 * way to allow for all the operations that streams require.
 */
public final class GlkFileStream implements GlkStream {

    private static Logger logger = Logger.getLogger("glk");
    private FileReference fileRef;
    private int fmode;
    private int rock;
    private boolean isUnicode;
    protected int _readCount;
    protected int _writeCount;
    private int _id;
    private RandomAccessFile realFile;

    public GlkFileStream(FileReference fileRef,
                         int fmode,
                         int rock,
                         boolean isUnicode) throws IOException {
        this.fileRef = fileRef;
        this.fmode = fmode;
        this.rock = rock;
        this.isUnicode = isUnicode;

        if (fileRef.fmode != 0 && fmode != fileRef.fmode) {
            logger.warning("FileStream FMODE != FileRef FMODE !!!");
        }
        logger.info(String.format("Opening file '%s' with usage: %d and fmode: %d",
                                  fileRef.file.getName(), fileRef.usage, fmode));

        realFile = new RandomAccessFile(fileRef.file, fileOpenMode());
        if (fmode == FileModes.WriteAppend) {
            realFile.seek(realFile.length());
        } else if (fmode == FileModes.Write) {
            // overwrite everything
            realFile.setLength(0);
        }
    }

    private String fileOpenMode() {
        return (fileRef.isReadOnly()) ? "r" : "rw";
    }

    public int style() {
        throw new UnsupportedOperationException("can not read style from file stream");
    }
    public void setStyle(int s) {
        throw new UnsupportedOperationException("can not set style in file stream");
    }

    public int id() { return _id; }
    public void setId(int id) { _id = id; }
    public int rock() { return rock; }

    public int size() {
        try {
            return (int) realFile.length();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
    public int position() {
        try {
            return (int) realFile.getFilePointer();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
    public void close() {
        try {
            realFile.close();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
    public int writeCount() { return _writeCount; }
    public void putChar(char c) {
        try {
            realFile.writeByte(c & 0xff);
            _writeCount++;
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
    public void putCharUni(int c) {
        try {
            realFile.writeChar(c);
            _writeCount++;
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
  
    public void seek(int newpos, int seekmode) {
        try {
            switch (seekmode) {
            case SeekModes.Start:   realFile.seek(newpos); break;
            case SeekModes.Current: realFile.seek(position() + newpos); break;
            case SeekModes.End:     realFile.seek(size() + newpos); break;
            default:
                throw new IllegalArgumentException(String.format("Unknown file seek mode: %d",
                                                                 seekmode));
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public void setHyperlink(int linkval) {
        throw new UnsupportedOperationException("setHyperlink not supported on file stream");
    }

    public int readCount() { return _readCount; }
    public int getChar() {
        try {
            if (position() >= realFile.length()) return -1;
            _readCount++;
            return (int) realFile.readByte();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public int getCharUni() {
        try {
            if (position() >= realFile.length()) return -1;
            _readCount++;
            return (int) realFile.readChar();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }
}
