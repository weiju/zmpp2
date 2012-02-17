/*
 * Created on 2012/02/17
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
package org.zmpp.iff;

import java.io.*;


/**
 * Generic implementation of the algorithm for compressing memory.
 */
public class QuetzalCompression {
    public static byte[] decompressDiffBytes(byte[] compressed,
                                             byte[] originalBytes,
                                             byte[] targetBytes,
                                             int numBytes) {
        System.arraycopy(originalBytes, 0, targetBytes, 0, numBytes);
        int compressedIndex = 0;
        int targetIndex = 0;
        while (compressedIndex < compressed.length) {
            if (compressed[compressedIndex] == 0) {
                int numZeroBytes = (compressed[compressedIndex + 1] & 0xff) + 1;
                targetIndex += numZeroBytes;
                compressedIndex++;
            } else {
                targetBytes[targetIndex] = (byte)
                    (originalBytes[targetIndex] ^ compressed[compressedIndex]);
                targetIndex++;
            }
            compressedIndex++;
        }
        return targetBytes;
    }

    private static void writeZeroRun(DataOutputStream out, int zeroCount) throws IOException {
        while (zeroCount > 256) {
            out.writeByte(0);
            out.writeByte(255);
            zeroCount -= 256;
        }
        if (zeroCount > 0) {
            out.writeByte(0);
            out.writeByte((byte) ((zeroCount - 1) & 0xff));
        }
    }


    public static byte[] compressDiffBytes(byte[] changedBytes,
                                           byte[] originalBytes,
                                           int numBytes) {
        try {
            ByteArrayOutputStream resultStream = new ByteArrayOutputStream();
            DataOutputStream out = new DataOutputStream(resultStream);
            int srcIndex = 0;
            int zeroCount = 0;
            while (srcIndex < numBytes) {
                int xorValue = (changedBytes[srcIndex] ^ originalBytes[srcIndex]) & 0xff;
                if (xorValue == 0) zeroCount++;
                else {
                    if (zeroCount > 0) { 
                        writeZeroRun(out, zeroCount);
                        zeroCount = 0;
                    }
                    out.writeByte(xorValue);
                }
                srcIndex++;
            }
            out.flush();
            out.close();
            return resultStream.toByteArray();
        } catch (IOException ex) {
            ex.printStackTrace(); // should not happen
        }
        return null;
    }
}
