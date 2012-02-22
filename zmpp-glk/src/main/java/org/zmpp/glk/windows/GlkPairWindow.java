/*
 * Created on 2012/02/21
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
package org.zmpp.glk.windows;

import org.zmpp.glk.io.NilStream;
import org.zmpp.glk.io.GlkStream;

/**
 * Pair windows only exist in the Glk windowing system. Since they only act as
 * inner nodes of the layout tree they do not have an UI equivalent.
 */
public class GlkPairWindow extends GlkWindow {

    public GlkWindow child0;
    public GlkWindow child1;
    public GlkWindow keyWindow; // key window child
    public int method;

    public GlkPairWindow(int id) {
        super(id, 0, 0);
    }

    public GlkStream outputStream() { return NilStream.getInstance(); }
    public int wintype() { return GlkWindowType.PairWindow; }
    public String typeName() { return "Pair"; }
    public boolean isTextBuffer() { return false; }
    public boolean isTextGrid() { return false; }
    public GlkStream echoStream() { return null; }
    public void setEchoStream(GlkStream stream) {
        throw new UnsupportedOperationException("Can not attach echo stream to pair window");
    }
 
    @Override public boolean isLeaf() { return false; }
    public int position() { return (method & GlkWindowPosition.Mask); }
    public int division() { return (method & GlkWindowDivision.Mask); }
    public boolean isProportional() { return division() == GlkWindowDivision.Proportional; }
    public boolean isFixed() { return division() == GlkWindowDivision.Fixed; }
    public boolean isLeft() { return position() == GlkWindowPosition.Left; }
    public boolean isRight() { return position() == GlkWindowPosition.Right; }
    public boolean isAbove() { return position() == GlkWindowPosition.Above; }
    public boolean isBelow() { return position() == GlkWindowPosition.Below; }
    public boolean isVertical() { return isAbove() || isBelow(); }
    public boolean isHorizontal() { return isLeft() || isRight(); }
}
