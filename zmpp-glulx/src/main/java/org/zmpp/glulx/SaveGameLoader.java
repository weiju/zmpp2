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


/**
 * The SaveGameLoader directly writes on the specified GlulxVMState object
 * to avoid unnecessary memory allocation.
 */
class SaveGameLoader {

    private static Logger logger = Logger.getLogger("glulx");
    private Glk          glk;
    private int          streamId;
    private GlulxVM      vm;
    private byte[]       originalRam;

    public SaveGameLoader(Glk glk, int streamId, GlulxVM vm,
                          byte[] originalRam) {
        this.glk         = glk;
        this.streamId    = streamId;
        this.vm          = vm;
        this.originalRam = originalRam;
    }

    private byte[] readId() {
        byte[] result = new byte[4];
        for (int i = 0; i < 4; i++) {
            result[i] = (byte) glk.get_char_stream(streamId);
        }
        return result;
    }

    private int readInt() {
        int result = 0;
        for (int i = 0; i < 4; i++) {
            char c = (char) glk.get_char_stream(streamId);
            result |= c << (8 * (3 - i));
        }
        return result;
    }

    private byte[] readFileBytes(int size) {
        glk.stream_set_position(streamId, 0, SeekModes.Start());
        byte[] result = new byte[size];
        for (int i = 0; i < size; i++) {
            result[i] = (byte) glk.get_char_stream(streamId);
        }
        return result;
    }

    private boolean isFormId(byte[] id) {
        return id[0] == (byte) 'F' && id[1] == (byte) 'O' &&
            id[2] == (byte) 'R' && id[3] == (byte) 'M';
    }

    private boolean isQuetzalFile(FormChunk formChunk) {
        return "IFZS".equals(formChunk.subId());
    }

    private void readCMemChunk(Chunk cmemChunk) {
        logger.info(String.format("Compressed memory, CHUNKSIZE = %d", cmemChunk.size()));
        int ramAddress = 0;
        int chunkEnd = cmemChunk.dataStart() + cmemChunk.size();
        int offset = cmemChunk.dataStart();

        // read new memory size and adjust VM state's memory size
        int memsize = cmemChunk.memory().intAt(offset);
        logger.info(String.format("NEW MEMSIZE = %d [OFFSET = %d]", memsize, offset));
        vm.setMemsize(cmemChunk.memory().intAt(offset));
        offset += 4;

        while (offset < chunkEnd) {
            int b = cmemChunk.memory().byteAt(offset);
            offset++;
            if (b == 0) {
                if (offset < chunkEnd) {
                    int len = cmemChunk.memory().byteAt(offset) + 1;
                    offset++;
                    ramAddress += len;
                } else {
                    ramAddress++;
                }
            } else {
                // XOR Delta-Value with initial RAM to get saved state
                vm.setRamByteAt(ramAddress, originalRam[ramAddress] ^ b);
                ramAddress++;
            }
        }
    }

    private void readUMemChunk(Chunk umemChunk) {
        logger.info(String.format("Uncompressed memory, SIZE = %d", umemChunk.size()));
        int offset = umemChunk.dataStart();
        int ramAddress = 0;
        int chunkEnd = umemChunk.dataStart() + umemChunk.size();
        vm.setMemsize(umemChunk.memory().intAt(offset));
        offset += 4;
        while (offset < chunkEnd) {
            vm.setRamByteAt(ramAddress, umemChunk.memory().byteAt(offset));
            ramAddress++;
            offset++;
        }
    }
                      
    private void readStksChunk(Chunk stksChunk) {
        int stackSize = stksChunk.size();
        logger.info(String.format("Uncompressed memory, SIZE = %d", stackSize));
        byte[] stackValues = new byte[stackSize];
        stksChunk.memory().copyBytesTo(stackValues, stksChunk.dataStart(), stackSize);
        vm.initStackFromByteArray(stackValues);
    }

    private void readMAllChunk(Chunk mallChunk) {
        logger.info(String.format("MAll Chunk found, SIZE = %d TODO", mallChunk.size()));
    }

    public boolean loadGame() {
        logger.info("LOAD_GAME");
        glk.stream_set_position(streamId, 0, SeekModes.End());
        int fileSize = glk.stream_get_position(streamId);
        byte[] fileBytes = readFileBytes(fileSize);
        try {
            DefaultFormChunk formChunk = new DefaultFormChunk(new DefaultMemory(fileBytes));
            if (isQuetzalFile(formChunk)) {
                // Quetzal file
                // we can ignore IFhd for Glulx games, instead, we might have
                // an MAll chunk
                Chunk cmemChunk = formChunk.subChunk("CMem");
                Chunk umemChunk = formChunk.subChunk("UMem");
                Chunk stksChunk = formChunk.subChunk("Stks");
                Chunk mallChunk = formChunk.subChunk("MAll");
                if (cmemChunk == null && umemChunk == null) {
                    logger.severe("NO MEMORY CHUNK FOUND !!");
                    return false;
                }
                if (stksChunk == null) {
                    logger.severe("NO STACK CHUNK FOUND !!");
                    return false;
                }
                
                logger.info("1. READING RAM");
                if (cmemChunk != null) readCMemChunk(cmemChunk);
                else if (umemChunk != null) readUMemChunk(umemChunk);
                logger.info("2. READING STACK STATE");
                readStksChunk(stksChunk);
                logger.info("3. READING HEAP");
                if (mallChunk != null) readMAllChunk(mallChunk);
                return true;
            } else {
                logger.severe("NOT A VALID QUETZAL FILE");
                return false;
            }
        } catch (java.io.IOException ex) {
            ex.printStackTrace();
            return false;
        }
    }
}
