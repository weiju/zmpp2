/*
 * Created on 2010/04/16
 * Copyright (c) 2010, Wei-ju Wu.
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
package org.zmpp.iff;

import org.zmpp.base.*;

class DefaultFormChunk extends DefaultChunk implements FormChunk {

    public DefaultFormChunk(Memory memory) throws java.io.IOException {
        super(memory, 0);
        if (!idAtAddressEquals(0, "FORM")) {
            throw new java.io.IOException(String.format("not a valid IFF format (ID was '%s')", id()));
        }
    }

    private boolean idAtAddressEquals(int address, String anId) {
        for (int i = 0; i < Chunk.IdLength; i++) {
            if (memory.byteAt(address + i) != anId.charAt(i)) return false;
        }
        return true;
    }

    private int subChunkAddress(String chunkId) {
        int currentAddr = Chunk.HeaderLength + Chunk.IdLength;
        int len = size();
        while (currentAddr < len) {
            if (idAtAddressEquals(currentAddr, chunkId)) return currentAddr;
            int chunkTotalSize = Chunk.HeaderLength +
                memory.intAt(currentAddr + Chunk.IdLength);
            chunkTotalSize += (chunkTotalSize % 2); // pad to even if necessary
            currentAddr += chunkTotalSize;
        }
        return -1;
    }

    // Public accessors
    public String subId() { return idAtOffset(Chunk.HeaderLength); }
    public boolean hasSubChunk(String chunkId) { return subChunkAddress(chunkId) != -1; }
    public Chunk chunkAtAddress(int chunkAddr) { return new DefaultChunk(memory, chunkAddr); }
  
    public int chunkDataSizeAtAddress(int chunkAddr) {
        return chunkAtAddress(chunkAddr).size();
    }

    public Chunk subChunk(String chunkId) {
        if (hasSubChunk(chunkId)) return chunkAtAddress(subChunkAddress(chunkId));
        else return null;
    }

    public Memory chunkDataForIdShared(String chunkId) {
        Chunk subChunk = chunkAtAddress(subChunkAddress(chunkId));
        return new DefaultMemory(memory.buffer(), 0, subChunk.size(), subChunk.dataStart());
    }

    public Memory chunkDataForId(String chunkId) {
        Chunk subChunk = chunkAtAddress(subChunkAddress(chunkId));
        byte[] zcodeBytes = new byte[subChunk.size()];
        System.arraycopy(memory.buffer(), subChunk.dataStart(), zcodeBytes, 0, subChunk.size());
        return new DefaultMemory0(zcodeBytes);
    }
}
