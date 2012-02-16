/*
 * Created on 2012/02/15
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

class LinearSearch extends GlulxArraySearch {

    public LinearSearch(GlulxVMState state) {
        super(state);
    }

    private boolean keyEqualsAtIndex(int compareIndex) {
        if (keyIndirect) {
            int addr0 = key;
            int addr1 = keyAddress(compareIndex);
            for (int i = 0; i < keySize; i++) {
                int b0 = state.memByteAt(addr0 + i);
                int b1 = state.memByteAt(addr1 + i);
                if (b0 != b1) return false;
            }
            return true;
        } else {
            int searchKey = truncateKey(key);
            int currentKey = keyAt(compareIndex);
            return currentKey == searchKey;
        }
    }
  
    private boolean doesCurrentKeyTerminate(int compareIndex) {
        if (!zeroKeyTerminates) return false;
        else if (keyIndirect) {
            int addr = keyAddress(compareIndex);
            for (int i = 0; i < keySize; i++) {
                int b = state.memByteAt(addr + i);
                if (b != 0) return false;
            }
            return true;
        } else return keyAt(compareIndex) == 0;
    }

    public int apply(int key, int keySize, int start,
                     int structSize, int numStructs, int keyOffset,
                     int options) {
        init(key, keySize, start, structSize, keyOffset, options);
        int i = 0;
        boolean cont = true;
        while (cont) {
            if (keyEqualsAtIndex(i)) return makeSearchResult(i);
            else if (doesCurrentKeyTerminate(i)) return makeSearchResult(-1);
            i++;
            cont = numStructs == -1 || numStructs >= 0 && i < numStructs;
        }
        return makeSearchResult(-1);
    }
}
