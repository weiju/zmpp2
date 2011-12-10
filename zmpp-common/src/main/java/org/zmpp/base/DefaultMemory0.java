/*
 * Created on 2011/12/09
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
package org.zmpp.base;

/**
 * Since memory access is one of the most frequent operations, it makes sense
 * to have a 0 based memory class, that doesn't need to calculate indexes.
 */
public class DefaultMemory0 implements Memory {
    private byte[] _buffer;

    public DefaultMemory0(byte[] data) { this._buffer = data; }
    public int size() { return _buffer.length; }
    public byte[] buffer() { return this._buffer; }
    public int byteAt(int addr) { return _buffer[addr] & 0xff; }
    public void setByteAt(int addr, int value) {
        _buffer[addr] = (byte) (value & 0xff);
    }
    public int shortAt(int addr) {
        return ((_buffer[addr] & 0xff) << 8) | (_buffer[addr + 1] & 0xff);
    }
    public void setShortAt(int addr, int value) {
        _buffer[addr]     = (byte) ((value >>> 8) & 0xff);
        _buffer[addr + 1] = (byte) (value & 0xff);
    }
    public int intAt(int addr) {
        return ((_buffer[addr] & 0xff) << 24) | ((_buffer[addr + 1] & 0xff) << 16) |
            ((_buffer[addr + 2] & 0xff) << 8) | (_buffer[addr + 3] & 0xff);
    }
    public void setIntAt(int addr, int value) {
        _buffer[addr]     = (byte) ((value >>> 24) & 0xff);
        _buffer[addr + 1] = (byte) ((value >>> 16) & 0xff);
        _buffer[addr + 2] = (byte) ((value >>> 8) & 0xff);
        _buffer[addr + 3] = (byte) (value & 0xff);
    }

    public void copyBytesTo(byte[] dest, int srcOffset, int numBytes) {
        if (dest.length < numBytes)
            throw new IllegalArgumentException("data array size too small");
        System.arraycopy(_buffer, srcOffset, dest, 0, numBytes);
    }
    public void copyBytesTo(int dstOffset, int srcOffset, int numBytes) {
        if ((_buffer.length < dstOffset + numBytes) ||
            (_buffer.length < srcOffset + numBytes))
            throw new IllegalArgumentException("copying over bounds");
        System.arraycopy(_buffer, srcOffset, _buffer, dstOffset, numBytes);
    }
    public void copyBytesFrom(byte[] src, int srcOffset, int dstOffset,
                              int numBytes) {
        System.arraycopy(src, srcOffset, _buffer, dstOffset, numBytes);
    }
    public boolean containsAddress(int addr) {
        return addr >= 0 && addr < _buffer.length;
    }
}
