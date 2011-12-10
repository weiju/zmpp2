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

// cheap stack implementation. This stack holds int's, but the only int
// value that might get stored is the return address in the call frame
// (only happens on a push).
public class IntStack {
    private int[] _values = new int[1024];
    private int _sp = 0;

    public int sp() { return _sp; }
    public void setSp(int value) { _sp = value; }
    public void push(int value) {
        _values[_sp] = value;
        _sp++;
    }
    public int pop() {
        _sp--;
        return _values[_sp] & 0xffff;
    }
    public int top() { return _values[_sp - 1] & 0xffff; }
    public void replaceTopWith(int value) {
        _values[_sp - 1] = value;
    }

    // there is only one single case where we need this function: return
    // PCs.
    public int value32At(int index) { return _values[index]; }
    public int valueAt(int index) { return _values[index] & 0xffff; }
    public void setValueAt(int index, int value) { _values[index] = value; }

    @Override public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < _sp; i++) {
            builder.append(String.format("%d ", _values[i]));
        }
        return builder.toString();
    }

    public int[] cloneValues() {
        int[] values = new int[_sp];
        for (int i = 0; i < _sp; i++) {
            values[i] = _values[i];
            i++;
        }
        return values;
    }

    public void initFromArray(int[] values) {
        for (int i = 0; i < values.length; i++) {
            _values[i] = values[i];
            i++;
        }
        _sp = values.length;
    }
}
