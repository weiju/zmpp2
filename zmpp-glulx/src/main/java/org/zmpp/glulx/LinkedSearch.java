/*
 * Created on 2012/02/14
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

class LinkedSearch extends GlulxSearch {

    private GlulxVM vm;

    public LinkedSearch(GlulxVM vm) {
        this.vm = vm;
    }

    private int keyAtAddress(int addr) {
        switch (keySize) {
        case 1: return vm.memByteAt(addr);
        case 2: return vm.memShortAt(addr);
        case 4: return vm.memIntAt(addr);
        default:
          throw new IllegalStateException("illegal key size: " + keySize);
        }
    }
  
    private boolean keyCompare(int key, int currentAddr) {
        if (keyIndirect) {
            for (int i = 0; i < keySize; i++) {
                int b0 = vm.memByteAt(key + i);
                int b1 = vm.memByteAt(currentAddr + keyOffset + i);
                if (b0 != b1) return false;
            }
            return true;
        } else {
            return truncateKey(key) == keyAtAddress(currentAddr + keyOffset);
        }
    }
  
    private boolean doesCurrentKeyTerminate(int currentAddr) {
        if (!zeroKeyTerminates) return false;
        else if (keyIndirect) {
            for (int i = 0; i < keySize; i++) {
                if (vm.memByteAt(currentAddr + keyOffset + i) != 0) return false;
            }
            return true;
        } else {
            return keyAtAddress(currentAddr + keyOffset) == 0;
        }
    }

    public int apply(int key, int keySize, int start,
                     int keyOffset, int nextOffset, int options) {
        super.init(key, keyOffset, keySize, options);

        if (returnIndex) {
            throw new UnsupportedOperationException("returnIndex not supported for linked search");
        }
        int currentAddr = start;
        int pos = 0;
        while (currentAddr != 0) {
            if (keyCompare(key, currentAddr)) return currentAddr;
            if (doesCurrentKeyTerminate(currentAddr)) return 0;
            pos++;
            currentAddr = vm.memIntAt(currentAddr + nextOffset);
        }
        return 0;
    }
}
