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
package org.zmpp.zcode;

abstract public class Alphabet {
    abstract public String table();
    abstract public String name();
    public char lookup(int zchar) { return table().charAt(zchar - 6); }
    public int charCodeFor(char c) {
        String tableString = table();
        for (int i = 0; i < tableString.length(); i++) {
            if (tableString.charAt(i) == c) return i + 6;
        }
        throw new IllegalArgumentException(String.format("Character '%c' not found", c));
    }
    public boolean contains(char c) {
        String tableString = table();
        for (int i = 0; i < tableString.length(); i++) {
            if (tableString.charAt(i) == c) return true;
        }
        return false;
    }
}

class Alphabet0 extends Alphabet {
    public String table() { return "abcdefghijklmnopqrstuvwxyz"; }
    public String name() { return "A0"; }

    private static Alphabet instance = new Alphabet0();
    public static Alphabet getInstance() { return instance; }
}

class Alphabet1 extends Alphabet {
    public String table() { return "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; }
    public String name() { return "A1"; }

    private static Alphabet instance = new Alphabet1();
    public static Alphabet getInstance() { return instance; }
}

class Alphabet2 extends Alphabet {
    public String table() { return " \n0123456789.,!?+#'\"/\\-:()"; }
    public String name() { return "A2"; }

    private static Alphabet instance = new Alphabet2();
    public static Alphabet getInstance() { return instance; }
}

class Alphabet2_V1 extends Alphabet {
    public String table() { return " 0123456789.,!?+#'\"/\\<-:()"; }
    public String name() { return "A2"; }

    private static Alphabet instance = new Alphabet2_V1();
    public static Alphabet getInstance() { return instance; }
}
