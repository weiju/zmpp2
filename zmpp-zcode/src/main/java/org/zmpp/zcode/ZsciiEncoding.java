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

public class ZsciiEncoding {

    private VMState vmState;
    private Alphabet A0 = Alphabet0.getInstance();
    private Alphabet A1 = Alphabet1.getInstance();
    private Alphabet A2 = Alphabet2.getInstance();
 
    public Alphabet currentAlphabet = A0;
    public Alphabet lastAlphabet    = A0;
    private int currentAbbreviation  = 0;
    public boolean decode10bit      = false;
    private int decode10bitStage     = 0;
    private int decode10bitFirst     = 0;
    public boolean shiftLock        = false;
    private AccentTable accentTable  = DefaultAccentTable.getInstance();

    public static final int NullChar = 0;
    public static final int AccentStart = 155;
    public static final int AccentEnd   = 251;

    public static int zsciiCodeFor(int c) { return c; }
    public static boolean isAccent(char c) {
        return c >= AccentStart && c <= AccentEnd;
    }
    
    public ZsciiEncoding(VMState state) {
        vmState = state;
    }

    public void reset() {
        currentAlphabet = A0;
        lastAlphabet    = A0;
        decode10bit     = false;
        shiftLock       = false;
        accentTable     = DefaultAccentTable.getInstance();
        // header is not available when constructor is called
        A2 = (vmState.header().version == 1) ? Alphabet2_V1.getInstance() :
            Alphabet2.getInstance();
    }

    public boolean isShiftCharacter(int zchar) {
        return zchar == 4 || zchar == 5 ||
            (vmState.header().version <= 2 && (zchar == 2 || zchar == 3));
    }

    private void handleShift(int zchar) {
        if (vmState.header().version <= 2) handleShiftV1V2(zchar);
        else handleShiftStd(zchar);
    }

    private void handleShiftV1V2(int zchar) {
        lastAlphabet = currentAlphabet;
        switch (zchar) {
        case 2:
            doShiftCode2();
            break;
        case 3:
            doShiftCode3();
            break;
        case 4:
            doShiftCode2();
            break;
        case 5:
            doShiftCode3();
            break;
        }
        shiftLock = (zchar == 4) || (zchar == 5);
    }

    private void doShiftCode2() {
        if (currentAlphabet == A0) currentAlphabet = A1;
        else if (currentAlphabet == A1) currentAlphabet = A2;
        else if (currentAlphabet == A2) currentAlphabet = A0;
    }

    private void doShiftCode3() {
        if (currentAlphabet == A0) currentAlphabet = A2;
        else if (currentAlphabet == A1) currentAlphabet = A0;
        else if (currentAlphabet == A2) currentAlphabet = A1;
    }

    private void handleShiftStd(int zchar) {
        //System.out.printf("handleShiftStd()");
        switch (zchar) {
        case 4:
            currentAlphabet = A1;
            break;
        case 5:
            currentAlphabet = A2;
            break;
        }
    }

    private void unshiftAlphabet() {
        currentAlphabet = (vmState.header().version <= 2) ? lastAlphabet : A0;
    }

    private boolean isV1Newline(int zchar) {
        return vmState.header().version == 1 && zchar == 1;
    }

    private boolean isAbbreviationCharacter(int zchar) {
        return (vmState.header().version >= 3 && zchar >= 1 && zchar <= 3) ||
        (vmState.header().version == 2 && zchar == 1);
    }

    public char zsciiToUnicode(char zsciiChar) {
        return isAccent(zsciiChar) ? accentTable.charAt(zsciiChar - AccentStart) : zsciiChar;
    }


    public void decodeZchar(int zchar, OutputStream stream) {
        if (currentAbbreviation != 0) {
            //System.out.printf("process abbreviation: %d zchar: %02x ALPHABET = %s\n",
            //                  currentAbbreviation, zchar, currentAlphabet.name());
            int entryNum = 32 * (currentAbbreviation - 1) + zchar;
            currentAbbreviation = 0; // mark abbreviation as processed
            int abbrevAddr = vmState.shortAt(vmState.header().abbrevTable() + entryNum * 2);

            // save old state
            Alphabet oldCurrentAlphabet = currentAlphabet;
            Alphabet oldLastAlphabet = lastAlphabet;
            boolean oldShiftLock = shiftLock;
            shiftLock = false;
            decodeZStringAtByteAddress(abbrevAddr * 2, stream);

            // restore old state
            currentAlphabet = oldCurrentAlphabet;
            lastAlphabet = oldLastAlphabet;
            shiftLock = oldShiftLock;
        } else if (decode10bit) {
            if (decode10bitStage == 2) {
                // second half of decode 10 bit
                int char10 = (decode10bitFirst << 5) | zchar;
                decode10bit = false;
                //System.out.printf("END 10 bit decoding, second: %02x, merged: %02x (%c)\n",
                //                  zchar, char10, char10);
                stream.putChar((char) char10);
            } else {
                decode10bitFirst = zchar;
                decode10bitStage ++;
                //System.out.printf("IN 10 bit decoding, first: %02x\n", zchar);
            }
        } else if (zchar == 0) stream.putChar(' ');
        else if (isV1Newline(zchar)) stream.putChar('\n');
        else if (isShiftCharacter(zchar)) handleShift(zchar);
        else if (isAbbreviationCharacter(zchar)) {
            if (currentAbbreviation == 0) currentAbbreviation = zchar;
        } else if (zchar == 6 && currentAlphabet == A2) {
            // 10-bit mode
            //System.out.println("START 10 bit MODE");
            decode10bit      = true;
            decode10bitStage = 1;
        } else if (zchar > 5) {
            stream.putChar(currentAlphabet.lookup(zchar));
            //System.out.printf("decoded ZCHAR '%c'\n", currentAlphabet.lookup(zchar));
        }
        // always reset the alphabet if not shift
        if (!shiftLock && !isShiftCharacter(zchar)) unshiftAlphabet();
    }

    public void decodeZString(OutputStream stream) {
        //vmState.pc += decodeZStringAtByteAddress(vmState.pc(), stream);
        vmState.incrementPC(decodeZStringAtByteAddress(vmState.pc(), stream));
    }

    public int decodeZStringAtByteAddress(int addr, OutputStream stream) {
        int numBytesDecoded = 0;
        int currentWord = vmState.shortAt(addr);
        boolean endofText = false;
        reset();

        while (!endofText) {
            numBytesDecoded += 2;
            //printf("CURRENT WORD: %02x (Alphabet: %s)\n", currentWord,
            //       currentAlphabet.name)
            int zchar0 = (currentWord >> 10) & 0x1f;
            int zchar1 = (currentWord >>  5) & 0x1f;
            int zchar2 = currentWord & 0x1f;
            //printf("c0 = %02x c1 = %02x c2 = %02x\n", zchar0, zchar1, zchar2)
            decodeZchar(zchar0, stream);
            decodeZchar(zchar1, stream);
            decodeZchar(zchar2, stream);
            endofText = (currentWord & 0x8000) == 0x8000;
            if (!endofText) {
                currentWord = vmState.shortAt(addr + numBytesDecoded);
            }
        }
        // not sure if we should always reset the decoder, but it seems to work
        // what happens in nested invocations (abbreviations ?)
        return numBytesDecoded;
    }

    public int decodeZStringAtPackedAddress(int paddr, OutputStream stream) {
        return decodeZStringAtByteAddress(vmState.header().unpackStringAddress(paddr), stream);
    }

    // This function returns the shift code:
    // 0: no shift needed - it's A0
    // 4: A1
    // 5: A2
    // -1 not in any alphabet -> 10bit code 
    public int shiftCodeFor(char c) {
        if (A0.contains(c)) return 0;
        else if (A1.contains(c)) return 4;
        else if (A2.contains(c)) return 5;
        else return -1;
    }

    public int charCodeFor(char c) {
        if (A0.contains(c)) return A0.charCodeFor(c);
        else if (A1.contains(c)) return A1.charCodeFor(c);
        else if (A2.contains(c)) return A2.charCodeFor(c);
        else {
            throw new IllegalArgumentException("char not found in alphabet");
        }
    }
}
