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

abstract class GlulxArraySearch extends GlulxSearch {

    protected int start;
    protected int structSize;
    protected GlulxVMState state;

    public GlulxArraySearch(GlulxVMState state) { this. state = state; }

    protected void init(int key, int keySize, int start,
                        int structSize, int keyOffset, int options) {
        super.init(key, keyOffset, keySize, options);
        this.start      = start;
        this.structSize = structSize;
    }

    protected int structAddress(int index) { return start + (index * structSize); }
    protected int keyAddress(int index) { return structAddress(index) + keyOffset; }

    protected int keyAt(int index) {
        int addr = keyAddress(index);
        switch (keySize) {
        case 1: return state.memByteAt(keyAddress(index));
        case 2: return state.memShortAt(keyAddress(index));
        case 4: return state.memIntAt(keyAddress(index));
        default:
            throw new IllegalStateException("illegal key size: " + keySize);
        }
    }
  
    /**
     * If returnIndex is true, returns the index of the result or -1.
     * If returnIndex is false, returns the address of the STRUCT found or 0.
     */
    protected int makeSearchResult(int index) {
        if (index == -1 && !returnIndex) return 0;
        else {
            return returnIndex ? index : structAddress(index);
        }
    }
}
