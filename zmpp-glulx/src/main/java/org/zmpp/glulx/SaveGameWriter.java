/*
 * Created on 2012/02/16
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

import java.util.logging.Logger;

import org.zmpp.base.*;
import org.zmpp.iff.*;
import org.zmpp.glk.*;


// Writes state to Quetzal file. Only writes UMem chunks, CMem is not
// necessary
class SaveGameWriter {

    private int bytesWritten;
    private Glk glk;
    private int streamId;
    private GlulxVM vm;
    private Operand storeLocation;

    public SaveGameWriter(Glk glk, int streamId, GlulxVM vm,
                          Operand storeLocation) {
        this.glk           = glk;
        this.streamId      = streamId;
        this.vm            = vm;
        this.storeLocation = storeLocation;
    }

    private void writeByteArray(byte[] bytes) {
        for (int i = 0; i < bytes.length; i++) {
            glk.put_char_stream(streamId, (char) (bytes[i] & 0xff));
        }
    }

    private void writeInt(int value) {
        glk.put_char_stream(streamId, (char) ((value >> 24) & 0xff));
        glk.put_char_stream(streamId, (char) ((value >> 16) & 0xff));
        glk.put_char_stream(streamId, (char) ((value >>  8) & 0xff));
        glk.put_char_stream(streamId, (char) ((value >>  0) & 0xff));
    }

    private void writeIffHeader() {
        glk.stream_set_position(streamId, 0, SeekModes.Start());
        writeByteArray("FORM".getBytes());
        writeInt(bytesWritten);
        writeByteArray("IFZS".getBytes()); // Quetzal ID
        bytesWritten += 4; // FORM and size not counted, data size starts at IFZS
    }
  
    private void writeIFhdChunk() {
        writeByteArray("IFhd".getBytes());
        writeInt(128);
        for (int i = 0; i < 128; i++) {
            glk.put_char_stream(streamId, (char) vm.memByteAt(i));
        }
        bytesWritten += 128 + 8;
    }

    private void writeUMemChunk() {
        int ramSize   = vm.ramSize();
        int chunkSize = ramSize + 4;
        byte[] destRam   = new byte[ramSize];
        writeByteArray("UMem".getBytes());
        writeInt(chunkSize);
        writeInt(vm.memsize());
        for (int i = 0; i < ramSize; i++) {
            glk.put_char_stream(streamId, (char) vm.ramByteAt(i));
        }
        bytesWritten += ramSize + 12;
    }

    private void writeStksChunk() {
        vm.pushCallStub(storeLocation);
        byte[] stackValues = vm.cloneStackValues();
        int stackSize   = stackValues.length;
        writeByteArray("Stks".getBytes());
        writeInt(stackSize);
        writeByteArray(stackValues);
        bytesWritten += stackSize + 8;
    }
    private void writeMAllChunk() { }
    private void writeIntDChunk() { }

    public boolean writeGameFile() {
        writeIffHeader();
        writeIFhdChunk();
        writeUMemChunk();
        writeStksChunk();
        writeIntDChunk();
        writeIffHeader(); // write again to log the file size
        return true;
    }
}
