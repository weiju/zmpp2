/*
 * Created on 2011/12/10
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

import org.zmpp.base.Memory;

class StoryHeader {
    private Memory story;
    private byte[] serial;
    // we make the version public, this is one of the most frequently used fields
    public int version;

    public StoryHeader(Memory story) {
        this.story = story;
        this.version = story.byteAt(0x00);
    }
    public int flags1() { return story.byteAt(0x01); }
    public int releaseNumber() { return story.shortAt(0x02); }
    public int himemStart() { return story.shortAt(0x04); }
    public int startPC() { return story.shortAt(0x06); }
    public int dictionary() { return story.shortAt(0x08); }
    public int objectTable() { return story.shortAt(0x0a); }
    public int globalVars() { return story.shortAt(0x0c); }
    public int staticStart() { return story.shortAt(0x0e); }
    public int flags2() { return story.shortAt(0x10); }
    public byte[] serialNumber() {
        if (serial == null) {
            serial = new byte[6];
            for (int i = 0; i < 6; i++) serial[i] = (byte) story.byteAt(0x12 + i);
        }
        return serial;
    }
    public int abbrevTable() { return story.shortAt(0x18); }
    public int fileLength() {
        int factor = 0;
        if (version <= 3) factor = 2;
        else if (version == 4 || version == 5) factor = 4;
        else factor = 8;
        return story.shortAt(0x1a) * factor;
    }

    public int checksum() { return story.shortAt(0x1c); }
    public int terpNumber() { return story.shortAt(0x1e); } // V4
    public int terpVersion() { return story.shortAt(0x1f); } // V4
    public int screenHeight() { return story.byteAt(0x20); } // V4
    public int screenWidth() { return story.byteAt(0x21); } // V4
    public int screenWidthUnits() { return story.shortAt(0x22); } // V5
    public int screenHeightUnits() { return story.shortAt(0x24); } // V5
    public int fontWidthUnits() {
        return (version == 6) ? story.shortAt(0x27) : story.shortAt(0x26);
    }
    public int fontHeightUnits() {
        return (version == 6) ? story.shortAt(0x26) : story.shortAt(0x27);
    }
    public int routinesOffset() { return story.shortAt(0x28); }
    public int staticStringsOffset() { return story.shortAt(0x2a); }
    public int defaultBackground() { return story.shortAt(0x2c); }
    public int defaultForeground() { return story.shortAt(0x2e); }
    public void setPixelWidthInStream3(int width) { story.setShortAt(0x30, width); }
    public void setStandardRevision(int revision) { story.setShortAt(0x32, revision); }
    public int alphabetTable() { return story.shortAt(0x34); }
    public int headerExtTable() { return story.shortAt(0x36); }
  
    public int unpackRoutineAddress(int addr) {
        switch (version) {
        case 1: return addr << 1;
        case 2: return addr << 1;
        case 3: return addr << 1;
        case 4: return addr << 2;
        case 5: return addr << 2;
        case 6: return (addr << 2) + (routinesOffset() << 3);
        case 7: return (addr << 2) + (routinesOffset() << 3);
        case 8: return addr << 3;
        default: return addr << 2;
        }
    }
    public int unpackStringAddress(int addr) {
        switch (version) {
        case 1: return addr << 1;
        case 2: return addr << 1;
        case 3: return addr << 1;
        case 4: return addr << 2;
        case 5: return addr << 2;
        case 6: return (addr << 2) + (staticStringsOffset() << 3);
        case 7: return (addr << 2) + (staticStringsOffset() << 3);
        case 8: return addr << 3;
        default: return addr << 2;
        }
    }
  
    public boolean isScoreGame() {
        return version < 3 || (flags1() & 0x02) == 0;
    }
}
