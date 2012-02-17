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

import java.io.InputStream;
import java.io.IOException;

/**
 * A helper class to efficently process BLORB data files.
 * Favor this over the slower BlorbData class.
 */
public class BlorbDataHelper {
    public static class ZStoryInfo {
        public int version;
        public int release;
        public String serial;
        public int checksum;

        public ZStoryInfo(int version, int release, String serial, int checksum) {
            this.version  = version;
            this.release  = release;
            this.serial   = serial;
            this.checksum = checksum;
        }
    }

    public static final int RIdx = 0x52496478;
    public static final int FORM = 0x464f524d;
    public static final int IFRS = 0x49465253;
    public static final int ZCOD = 0x5a434f44;

    private static final int BufferSize = 1024;
    private static final int HeaderSize = 52;
    private static int chunkId;
    private static int chunklen;

    // Checking for valid Blorb files
    public static boolean isIffFile(byte[] dataBytes) { return intAt(dataBytes, 0) == FORM; }
    public static boolean isBlorbFile(byte[] dataBytes) {
        return isIffFile(dataBytes) && intAt(dataBytes, 8) == IFRS;
    }

    private static int intAt(byte[] dataBytes, int offset) {
        return ((dataBytes[offset] & 0xff) << 24) | ((dataBytes[offset + 1] & 0xff) << 16) |
            ((dataBytes[offset + 2] & 0xff) << 8) | (dataBytes[offset + 3] & 0xff);
    }

    private static int shortAt(byte[] dataBytes, int offset) {
        return ((dataBytes[offset] & 0xff) << 8) | (dataBytes[offset + 1] & 0xff);
    }

    // A faster, less memory-consuming version of a Z info scan
    // which can be used to retrieve the information in a batch, possibly
    // on a memory-restricted environment. We just read information
    // on the fly and just use the header information we need
    // Currently, this can read Z-blorbs and plain Z-files
    public static ZStoryInfo quickScanZInfo(InputStream inputStream) throws IOException {
        byte[] buffer = new byte[BufferSize];

        // read the identifier
        int bytesRead = inputStream.read(buffer, 0, 12);
        int bytesReadSoFar = bytesRead;

        if (isBlorbFile(buffer)) {
            int filelen = intAt(buffer, 4);
            chunkId = 0;
            chunklen = 0;
            while (chunkId != ZCOD && bytesReadSoFar < filelen) {
                readChunkHeader(inputStream, buffer);
                bytesReadSoFar += 8;
                if (chunkId != ZCOD) {
                    // pad to even
                    int bytesToRead = (chunklen % 2 == 1) ? chunklen + 1 : chunklen;
                    while (bytesToRead > 0) {
                        int readlen = Math.min(bytesToRead, BufferSize);
                        bytesRead = inputStream.read(buffer, 0, readlen);
                        bytesToRead -= bytesRead;
                        bytesReadSoFar += bytesRead;
                    }
                }
            }
            if (bytesReadSoFar < filelen) {
                inputStream.read(buffer, 0, HeaderSize);
                return zinfo(buffer);
            } else throw new IOException("no ZCOD chunk found");
        } else {
            // just assume we are a Z-code file
            inputStream.read(buffer, 12, HeaderSize - 12);
            return zinfo(buffer);
        }
    }

    private static ZStoryInfo zinfo(byte[] buffer) {
        int version = buffer[0];
        StringBuilder serialBuffer = new StringBuilder();
        for (int i = 0; i < 6; i++) {
            serialBuffer.append((char) buffer[0x12 + i]);
        }
        String serial = (version > 1) ? serialBuffer.toString() : null;
        return new ZStoryInfo(version, shortAt(buffer, 2), serial, shortAt(buffer, 0x1c));
    }

    private static void readChunkHeader(InputStream inputStream, byte[] buffer) throws IOException {
        if (inputStream.read(buffer, 0, 8) == 8) {
            chunkId = intAt(buffer, 0);
            chunklen = intAt(buffer, 4);
        } else throw new IOException("error reading Blorb file");
    }
}
