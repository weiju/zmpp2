/*
 * Created on 2012/02/16
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
package org.zmpp.base;

/**
 * Memory implemented with a byte array
 * All byte and short are considered to be unsigned values, int's are signed
 * to avoid extension to long values where in most cases, we won't use the full
 * positive range of a 32-bit int
 */
public class DefaultMemory implements Memory, Comparable<DefaultMemory> {

    public byte[] buffer;
    public int address;
    private int _size;
    private int _startIndex;

    public DefaultMemory(byte[] buffer, int address, int size,
                         int startIndex) {
        this.buffer     = buffer;
        this.address    = address;
        this._size       = size;
        this._startIndex = startIndex;
    }

    public DefaultMemory(byte[] arr) { this(arr, 0, arr.length, 0); }
    public DefaultMemory(byte[] arr, int addr) { this(arr, addr, arr.length, 0); }

    public static DefaultMemory create(int address, int size) {
        return new DefaultMemory(new byte[size], address, size, 0);
    }
    public int size()      { return _size; }
    public byte[] buffer() { return buffer; }
    private int indexForAddress(int addr) { return addr - address + _startIndex; }

    public int byteAt(int addr) {
        return buffer[indexForAddress(addr)] & 0xff;
    }

    public void setByteAt(int addr, int value) {
        buffer[indexForAddress(addr)] = (byte) (value & 0xff);
    }

    public int shortAt(int addr) {
        int offset = indexForAddress(addr);
        return ((buffer[offset] & 0xff) << 8) | (buffer[offset + 1] & 0xff);
    }

    public void setShortAt(int addr, int value) {
        int offset = indexForAddress(addr);
        buffer[offset]     = (byte) ((value >>> 8) & 0xff);
        buffer[offset + 1] = (byte) (value & 0xff);
    }

    public int intAt(int addr) {
        int offset = indexForAddress(addr);
        return ((buffer[offset] & 0xff) << 24) | ((buffer[offset + 1] & 0xff) << 16) |
            ((buffer[offset + 2] & 0xff) << 8) | (buffer[offset + 3] & 0xff);
    }

    public void setIntAt(int addr, int value) {
        int offset = indexForAddress(addr);
        buffer[offset]     = (byte) ((value >>> 24) & 0xff);
        buffer[offset + 1] = (byte) ((value >>> 16) & 0xff);
        buffer[offset + 2] = (byte) ((value >>> 8) & 0xff);
        buffer[offset + 3] = (byte) (value & 0xff);
    }

    public void copyBytesTo(byte[] dest, int srcOffset, int numBytes) {
        if (dest.length < numBytes) {
            throw new IllegalArgumentException("data array size too small");
        }
        System.arraycopy(buffer, indexForAddress(srcOffset), dest, 0, numBytes);
    }

    public void copyBytesTo(int dstOffset, int srcOffset, int numBytes) {
        int srcIndex  = indexForAddress(srcOffset);
        int destIndex = indexForAddress(dstOffset);
        if ((buffer.length < destIndex + numBytes) ||
            (buffer.length < srcIndex  + numBytes)) {
            throw new IllegalArgumentException("copying over bounds");
        }
        System.arraycopy(buffer, srcIndex, buffer, destIndex, numBytes);
    }

    public void copyBytesFrom(byte[] src, int srcOffset, int destOffset,
                              int numBytes) {
        System.arraycopy(src, srcOffset, buffer, indexForAddress(destOffset), numBytes);
    }

    public boolean containsAddress(int addr) { return addr >= address && addr < address + _size; }
    public int compareTo(DefaultMemory other) { return address - other.address; }

    public Memory littleEndian() {
        // this currently does not do anything, but it should. The TADS interpreter
        // uses this. Instead, there should be a specialized Little Endian Memory class.
        return this;
    }
}
