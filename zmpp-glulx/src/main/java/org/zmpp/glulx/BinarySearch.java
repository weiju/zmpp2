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

class BinarySearch extends GlulxArraySearch {

    public BinarySearch(GlulxVMState state) {
        super(state);
    }

    private int keyCompareAtIndex(int compareIndex) {
        if (keyIndirect) {
            // indirect search - this means we need to compare lexicographical
            // in memory on an arbitrary key size
            int addr0 = key;
            int addr1 = keyAddress(compareIndex);

            for (int i = 0; i < keySize; i++) {
                int b0 = state.memByteAt(addr0 + i);
                int b1 = state.memByteAt(addr1 + i);
                if (b0 < b1)      return -1;
                else if (b0 > b1) return 1;
            }
            return 0;
        } else {
            // compare as unsigned long values, to prevent funny effects
            // Java ints are always signed
            long currentKey = ((long) keyAt(compareIndex)) & 0x0ffffffffl;
            long directKey  = ((long) truncateKey(key)) & 0x0ffffffffl;
            if      (currentKey == directKey) return 0;
            else if (directKey < currentKey)  return -1;
            else                              return 1;
        }
    }

    private int binsearch(int left, int right) {
        if (left > right) return makeSearchResult(-1);
        int middle = left + (right - left) / 2;
        int comp = keyCompareAtIndex(middle);
        if (comp == 0) return makeSearchResult(middle);
        if (comp < 0)  return binsearch(left, middle - 1);
        else           return binsearch(middle + 1, right);
    }

    public int apply(int key, int keySize, int start,
                     int structSize, int numStructs, int keyOffset,
                     int options) {
        init(key, keySize, start, structSize, keyOffset, options);
        if (zeroKeyTerminates) {
            throw new UnsupportedOperationException("zeroKeyTerminates not allowed for binary search");
        }
        return binsearch(0, numStructs - 1);
    }
}
