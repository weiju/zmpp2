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

import org.zmpp.glk.Glk;

class GlkIOSystem extends IOSystem {

    private Glk glk;

    public GlkIOSystem(GlulxVM vm, Glk glk, int rock) {
        super(vm, rock);
        this.glk = glk;
    }

    public int id() { return 2; }
    public void streamChar(char c) { glk.put_char(c); }
    public void streamUniChar(int c) { glk.put_char_uni(c); }
  
    // streamstr actions
    public StreamStrState handleChar8(char c, boolean inBetween,
                                      int currentStreamByte,
                                      int currentStreamBit) {
        glk.put_char(c);
        return StreamStrState.Continue;
    }

    public StreamStrState handleChar32(int c, boolean inBetween,
                                       int currentStreamByte,
                                       int currentStreamBit) {
        glk.put_char_uni(c);
        return StreamStrState.Continue;
    }
  
    public StreamStrState handleHuffmanCString(int nodeAddr,
                                               int currentStreamByte,
                                               int currentStreamBit,
                                               boolean inBetween) {
        int cstrAddr    = nodeAddr + 1;
        int currentChar = vm.memByteAt(cstrAddr);
        while (currentChar != 0) {
            streamChar((char) currentChar);
            cstrAddr++;
            currentChar = vm.memByteAt(cstrAddr);
        }
        return StreamStrState.Continue;
    }

    public StreamStrState handleHuffmanUnicodeString(int nodeAddr,
                                                     int currentStreamByte,
                                                     int currentStreamBit,
                                                     boolean inBetween) {
        int uniCStrAddr = nodeAddr + 1;
        int currentChar = vm.memIntAt(uniCStrAddr);
        while (currentChar != 0) {
            streamChar((char) (currentChar & 0xffff));
            uniCStrAddr += 4;
            currentChar = vm.memIntAt(uniCStrAddr);
        }
        return StreamStrState.Continue;
    }
}
