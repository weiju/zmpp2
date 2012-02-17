/*
 * Created on 2012/02/15
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

import java.util.logging.*;

/**
 * The super class of IO systems, it defines the public/overridable methods
 * for streamstr, streamnum, streamchar and streamunichar and also contains
 * the huffman decoding algorithm.
 *
 * Design notes:
 * - we do not build an explicit tree structure and walk the memory structure
 *   directly (simpler and more general, to support different environments)
 * - externalized state: State is not kept in the decoder class, instead,
 *   print state objects are passed within the print process and in the suspend
 *   and resume cycle
 * - use of polymorphism: the Strategy pattern was used to implement different
 *   behaviors of the IOsystems.
 * - Huffman decoding incorporated: the Huffman decompression algorithm was
 *   originally in its own class, but was merged into IOSystem, because it
 *   also behaves different depending on the IOSystem
 *
 * Open question:
 * - what advantages would changing the state objects to use the State pattern
 *   have ?
 */

abstract class IOSystem {

    protected static Logger logger = Logger.getLogger("glulx");
    public abstract int id();
    public abstract void streamChar(char c);
    public abstract void streamUniChar(int c);
    protected GlulxVM vm;
    private GlulxVMState vmState;
    public int rock;

    public IOSystem(GlulxVM vm, int rock) {
        this.vm      = vm;
        this.vmState = vm.state;
        this.rock    = rock;
    }

    // Hm, this seems kind of not-documented: non-filter stream_num can also start
    // at any position.
    public void streamNum(int num, int pos) {
        String numberString = String.format("%d", num);
        for (int i = pos; i < numberString.length(); i++) {
            streamChar(numberString.charAt(i));
        }

        if (pos > 0) {
            int fpVal    = vmState.popInt();
            int pcVal    = vmState.popInt();
            int destAddr = vmState.popInt();
            int destType = vmState.popInt();
            if (destType == DestTypes.ResumeExecuteFunction) {
                vmState.pc = pcVal;
                vmState.fp = fpVal;
            } else {
                throw new IllegalStateException(String.format("FALLBACK, SHOULD NOT HAPPEN, destType is: %d",
                                                              destType));
            }
        }
    }

    public void streamStr(StreamStrState state) {
        StreamStrState currentState = state;
        while (!currentState.suspendPrinting()) {
            currentState = streamStrIteration(currentState);
        }
    }

    private StreamStrState popCallStubIfNecessary(boolean inBetween, StreamStrState state) {
        if (inBetween && state == StreamStrState.Finished) {
            int fpVal    = vmState.popInt();
            int pcVal    = vmState.popInt();
            int destAddr = vmState.popInt();
            int destType = vmState.popInt();
            if (destType == DestTypes.ResumeExecuteFunction) {
                vmState.pc = pcVal;
                vmState.fp = fpVal;
                return StreamStrState.Finished;
            } else if (destType == DestTypes.ResumePrintCompressed) {
                return StreamStrState.resumeAt(pcVal, destAddr);
            } else {
                logger.severe(String.format("FALLBACK, SHOULD NOT HAPPEN, destType is: %d", destType));
                return StreamStrState.Finished;
            }
        } else return state;
    }

    private StreamStrState streamStrIteration(StreamStrState state) {
        boolean inBetween = state.printSubstring() || state.resumeDecoding() ||
            state.resumeCString() || state.resumeUniString();
        StreamStrState nextState = null;
        if (state.startString() || state.printSubstring()) {
            // this conditional branch processes strings only from their start position
            int stringAddress = state.byteAddress;
            int strtype = vmState.memByteAt(stringAddress);
            switch (strtype) {
            case 0xe0: nextState = handleStreamstrCString(inBetween, stringAddress); break;
            case 0xe1: nextState = decompress(inBetween, stringAddress + 1, 0); break;
            case 0xe2: nextState = handleStreamstrUnicodeString(inBetween, stringAddress); break;
            default:
                throw new IllegalStateException(String.format("unknown string type: %02x", strtype));
            }
        } else if (state.resumeDecoding()) { // continue Huffman decoding in the middle
            nextState = decompress(inBetween, state.byteAddress, state.bitnum);
        } else if (state.resumeCString()) {
            nextState = handleResumeCString(state.byteAddress);
        } else if (state.resumeUniString()) {
            nextState = handleResumeUniString(state.byteAddress);
        } else throw new IllegalArgumentException("Illegal continuation status: " + state.status);
        return popCallStubIfNecessary(inBetween, nextState);
    }

    public abstract StreamStrState handleChar8(char c, boolean inBetween,
                                               int currentStreamByte,
                                               int currentStreamBit);
    public abstract StreamStrState handleChar32(int c, boolean inBetween,
                                                int currentStreamByte,
                                                int currentStreamBit);

    public StreamStrState handleStreamstrCString(boolean inBetween, int stringAddress) {
        int currentAddr = stringAddress + 1;
        int currentChar = vmState.memByteAt(currentAddr);
        while (currentChar != 0) {
            streamChar((char) currentChar);
            currentAddr++;
            currentChar = vmState.memByteAt(currentAddr);
        }
        return StreamStrState.Finished;
    }
  
    public StreamStrState handleStreamstrUnicodeString(boolean inBetween, int stringAddress) {
        int currentAddr = stringAddress + 4;
        int currentChar = vmState.memIntAt(currentAddr);
        while (currentChar != 0) {
            streamUniChar((char) (currentChar & 0xffff));
            currentAddr += 4;
            currentChar = vmState.memIntAt(currentAddr);
        }
        return StreamStrState.Finished;
    }
  
    public StreamStrState handleResumeUniString(int addr) {
        throw new UnsupportedOperationException("resuming Unicode string not supported by default");
    }
    public StreamStrState handleResumeCString(int addr) {
        throw new UnsupportedOperationException("resuming Unicode string not supported by default");
    }

    // ************************************************************************
    // ****** Huffman Decoding
    // ******************************

    private boolean isStringType(int valType) { return valType >= 0xe0; }

    private int suspendPrintCompressed(int ref,
                                       boolean inBetween,
                                       int currentStreamByte, int currentStreamBit) {
        if (!inBetween) {
            vmState.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                                  vmState.pc, vmState.fp);
        }
        vmState.pushCallStub(DestTypes.ResumePrintCompressed, currentStreamBit,
                             currentStreamByte, vmState.fp);
        return vmState.memByteAt(ref);
    }

    private StreamStrState handleIndirectReference(int ref,
                                                   boolean inBetween,
                                                   int currentStreamByte,
                                                   int currentStreamBit) {
        int refType = suspendPrintCompressed(ref, inBetween, currentStreamByte, currentStreamBit);
        if (isStringType(refType)) {
            return StreamStrState.substring(ref);
        } else {
            vm.callWithArgsNoCallStub(ref);
            return StreamStrState.CallFunction;
        }
    }

    private StreamStrState handleIndirectReferenceWithArgs(int ref, int[] args,
                                                           boolean inBetween,
                                                           int currentStreamByte,
                                                           int currentStreamBit) {
        int refType = suspendPrintCompressed(ref, inBetween, currentStreamByte, currentStreamBit);
        if (isStringType(refType)) {
            if (refType != 0xe1) {
                throw new UnsupportedOperationException("Uncompressed strings not supported in substrings");
            }
            return StreamStrState.substring(ref);
        } else {
            vm.callWithArgsNoCallStub(ref, args);
            return StreamStrState.CallFunction;
        }
    }
  
    public abstract StreamStrState handleHuffmanCString(int nodeAddr,
                                                        int currentStreamByte,
                                                        int currentStreamBit,
                                                        boolean inBetween);
    public abstract StreamStrState handleHuffmanUnicodeString(int nodeAddr,
                                                              int currentStreamByte,
                                                              int currentStreamBit,
                                                              boolean inBetween);

    private StreamStrState handleLeaf(int nodeType, int nodeAddr,
                                      boolean inBetween,
                                      int currentStreamByte,
                                      int currentStreamBit) {
        switch (nodeType) {
        case 0x02: // C character
            return handleChar8((char) vmState.memByteAt(nodeAddr + 1),
                               inBetween, currentStreamByte, currentStreamBit);
        case 0x03:
            return handleHuffmanCString(nodeAddr, currentStreamByte, currentStreamBit,
                                        inBetween);
        case 0x04: // unicode character
            return handleChar32(vmState.memIntAt(nodeAddr + 1),
                                inBetween, currentStreamByte, currentStreamBit);
        case 0x05:
            return handleHuffmanUnicodeString(nodeAddr,
                                              currentStreamByte, currentStreamBit,
                                              inBetween);
        case 0x08: // indirect reference
            return handleIndirectReference(vmState.memIntAt(nodeAddr + 1),
                                           inBetween,
                                           currentStreamByte,
                                           currentStreamBit);
        case 0x09: // double indirect reference
            {
                int refptr = vmState.memIntAt(nodeAddr + 1);
                return handleIndirectReference(vmState.memIntAt(refptr),
                                               inBetween,
                                               currentStreamByte,
                                               currentStreamBit);
            }
        case 0x0a: // indirect reference with arguments
            {
                int ref = vmState.memIntAt(nodeAddr + 1);
                int argCount = vmState.memIntAt(nodeAddr + 5);
                int[] args = new int[argCount];

                for (int i = 0; i < argCount; i++) {
                    args[i] = vmState.memIntAt(nodeAddr + 9 + i * 4);
                }
                return handleIndirectReferenceWithArgs(ref, args,
                                                       inBetween,
                                                       currentStreamByte,
                                                       currentStreamBit);
            }
        case 0x0b: // double indirect reference with arguments
            {
                int refptr = vmState.memIntAt(nodeAddr + 1);
                int argCount = vmState.memIntAt(nodeAddr + 5);
                int[] args = new int[argCount];
                for (int i = 0; i < argCount; i++) {
                    args[i] = vmState.memIntAt(nodeAddr + 9 + i * 4);
                }
                return handleIndirectReferenceWithArgs(vmState.memIntAt(refptr), args,
                                                       inBetween,
                                                       currentStreamByte,
                                                       currentStreamBit);
            }
        default:
            throw new IllegalArgumentException(String.format("unsupported node type: %02x", nodeType));
        }
    }

    // This is a stateful computation: After decompress() exits due to a function
    // call, another call to decompress() picks up decoding from where the
    // previous decompress() invocation left off.
    private StreamStrState decompress(boolean inBetween, int streamByte, int bit) {
        int decodeTable           = vm.currentDecodingTable;
        int tableLength           = vmState.memIntAt(decodeTable);
        int numNodes              = vmState.memIntAt(decodeTable + 4);
        int rootNodeAddr          = vmState.memIntAt(decodeTable + 8);
        int streamAddr            = streamByte;
        int bitnum                = bit;
    
        boolean functionWasCalled = false;
        int currentNodeAddr       = rootNodeAddr;
        StreamStrState result     = StreamStrState.Continue;

        // this value holds the byte sized window into the bitstream
        int currentByte = vmState.memByteAt(streamAddr) >> bitnum;

        while (result == StreamStrState.Continue) {
            if ((currentByte & 1) == 0) {
                currentNodeAddr = vmState.memIntAt(currentNodeAddr + 1);
            } else {
                currentNodeAddr = vmState.memIntAt(currentNodeAddr + 5);
            }
            int nodeType   = vmState.memByteAt(currentNodeAddr);
            boolean atLeaf = nodeType != 0x00;
            if (nodeType == 0x01) result = StreamStrState.Finished;

            // next bit in stream
            if (result == StreamStrState.Continue) {
                bitnum++;
                currentByte >>= 1;
                if (bitnum == 8) {
                    bitnum = 0;
                    streamAddr++;
                    currentByte = vmState.memByteAt(streamAddr);
                }
      
                if (atLeaf) {
                    result = handleLeaf(nodeType, currentNodeAddr,
                                        inBetween, streamAddr, bitnum);
                    currentNodeAddr = rootNodeAddr;
                }
            }
        }
        return result;
    }
}
