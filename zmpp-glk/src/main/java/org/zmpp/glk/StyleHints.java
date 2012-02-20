/*
 * Created on 2012/02/20
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
package org.zmpp.glk;

import static org.zmpp.glk.StyleType.*;
import static org.zmpp.glk.StyleHintType.*;
import static org.zmpp.glk.StyleHintJustification.*;

public final class StyleHints {

    public static final int[][] Defaults = {
        {0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0}, // Normal
        {0, 0, LeftFlush, 0, 1, 0, 1, -1, -1, 0}, // Emphasized
        {0, 0, LeftFlush, 0, 0, 0, 0, -1, -1, 0}, // Preformatted
        {0, 0, LeftFlush, 1, 1, 0, 1, -1, -1, 0}, // Header
        {0, 0, LeftFlush, 0, 1, 1, 1, -1, -1, 0}, // Subheader
        {0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0}, // Alert
        {0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0}, // Note
        {0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0}, // BlockQuote
        {0, 0, LeftFlush, 0, 1, 0, 1, -1, -1, 0}, // Input
        {0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0}, // User1
        {0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0}  // User2
    };

    private int[][] hints = new int[StyleType.Num][StyleHintType.Num];

    public StyleHints() { reset(); }
    public void reset() {
        for (int styleNum = 0; styleNum < StyleType.Num; styleNum++) {
            for (int hintNum = 0; hintNum < StyleHintType.Num; hintNum++) {
                reset(styleNum, hintNum);
            }
        }
    }
    public void reset(int styleNum, int hintNum) {
        hints[styleNum][hintNum] = StyleHints.Defaults[styleNum][hintNum];
    }
    public int get(int styleNum, int hintNum) {
        if (styleNum >= 0 && styleNum < StyleType.Num &&
            hintNum >= 0 && hintNum < StyleHintType.Num) {
            return hints[styleNum][hintNum];
        } else return -1;
    }
    public void set(int styleNum, int hintNum, int value) {
        if (styleNum >= 0 && styleNum < StyleType.Num &&
            hintNum >= 0 && hintNum < StyleHintType.Num) {
            hints[styleNum][hintNum] = value;
        }
    }
    public boolean distinguishable(int style1, int style2) {
        if (style1 == style2) return false;
        else if (style1 < 0 || style2 < 0 ||
                 style1 >= StyleType.Num || style2 >= StyleType.Num) return false;
        else {
            for (int hintNum = 0; hintNum < StyleHintType.Num; hintNum++) {
                if (hints[style1][hintNum] != hints[style2][hintNum]) return true;
            }
            return false;
        }
    }
}
