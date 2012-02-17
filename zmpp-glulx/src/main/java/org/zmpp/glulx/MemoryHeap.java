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
package org.zmpp.glulx;

import java.util.List;
import java.util.ArrayList;
import java.util.logging.*;
import org.zmpp.base.*;

/**
 * Implementation of the Glulx Heap. The current implementation works like this:
 * <ol>
 *   <li>
 *     Always allocate from the highest current address and append new blocks
 *     to the end of the memory block list. This ensures that the list is always
 *     sorted in ascending address order, so we can perform binary search on it
 *     This means allocate() is O(1)
 *   </li>
 *   <li>
 *     Lookup and removal are performed using binary search, so these operations
 *     can be done in O(log n)
 *   </li>
 *   <li>
 *     The binary search we use compares an address with the range that
 *     a memory block represents.
 *   </li>
 *  </ol>
 */
class MemoryHeap implements Memory {

    private static Logger logger = Logger.getLogger("glulx");
    public int address;
    private List<DefaultMemory> _memoryBlocks = new ArrayList<DefaultMemory>();
    private int _highAddress;

    public MemoryHeap(int address) {
        this.address      = address;
        this._highAddress = address;
    }

    private int compareAddress(int addr, DefaultMemory memblock) {
        if (addr < memblock.address)                        return -1;
        else if (addr >= memblock.address &&
                 addr < memblock.address + memblock.size()) return 0;
        else                                                return 1;
    }

    private DefaultMemory memblockAtRecursive(int addr, int left, int right) {
        if (left > right) return null;
        else {
            int middle = left + (right - left) / 2;
            int compVal = compareAddress(addr, _memoryBlocks.get(middle));
            if (compVal == 0) return _memoryBlocks.get(middle);
            else if (compVal < 0) return memblockAtRecursive(addr, left, middle - 1);
            else return memblockAtRecursive(addr, middle + 1, right);
        }
    }

    public byte[] buffer() {
        throw new UnsupportedOperationException("buffer() not supported");
    }

    public int size() { return 0; } // undefined
    public DefaultMemory memblockAt(int addr) {
        DefaultMemory block = memblockAtRecursive(addr, 0, _memoryBlocks.size() - 1);
        //printf("SEARCH BLOCK AT ADDR $%02x FOUND: %b\n", addr, block != null)
        return block;
    }
  
    public int maxAddress() { return _highAddress; }
    public int allocate(int size) {
        int blockAddress = _highAddress;
        logger.info(String.format("ALLOCATE HEAP MEM WITH SIZE: %d ADDR: $%02x", size, blockAddress));
        _highAddress += size;
        DefaultMemory block = DefaultMemory.create(blockAddress, size);
        _memoryBlocks.add(block);
        return blockAddress;
    }
  
    public void free(int addr) {
        logger.info(String.format("FREE HEAP MEM AT ADDR: $%02x", addr));
        _memoryBlocks.remove(memblockAt(addr));
    }

    public boolean active() { return _memoryBlocks.size() > 0; }

    // Memory interface, map to allocated memory blocks
    public int byteAt(int addr) { return memblockAt(addr).byteAt(addr); }
    public void setByteAt(int addr, int value) { memblockAt(addr).setByteAt(addr, value); }
    public int shortAt(int addr) { return memblockAt(addr).shortAt(addr); }
    public void setShortAt(int addr, int value) { memblockAt(addr).setShortAt(addr, value); }
    public int intAt(int addr) { return memblockAt(addr).intAt(addr); }
    public void setIntAt(int addr, int value) { memblockAt(addr).setIntAt(addr, value); }

    // copying data
    public void copyBytesTo(byte[] dest, int srcOffset, int numBytes) {
        memblockAt(srcOffset).copyBytesTo(dest, srcOffset, numBytes);
    }
    public void copyBytesTo(int dstOffset, int srcOffset, int numBytes) {
        memblockAt(srcOffset).copyBytesTo(dstOffset, srcOffset, numBytes);
    }
    public void copyBytesFrom(byte[] src, int srcOffset, int destOffset, int numBytes) {
        memblockAt(destOffset).copyBytesFrom(src, srcOffset, destOffset, numBytes);
    }
}
