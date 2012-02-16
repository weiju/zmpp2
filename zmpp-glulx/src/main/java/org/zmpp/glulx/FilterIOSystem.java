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

class FilterIOSystem extends IOSystem {
    public int funType;
    private int functionAddr;

    public FilterIOSystem(GlulxVM vm, int functionAddr) {
        super(vm, functionAddr); // note that functionAddr and rock are identical
        this.functionAddr = functionAddr;
        funType = vm.state.memByteAt(functionAddr);
    }

    public int id() { return 1; }

    public void streamChar(char c) {
        vm.callWithArgs(DestTypes.DoNotStore, 0, vm.state.pc, vm.state.fp, functionAddr, (int) c);
    }

    public void streamUniChar(int c) {
        vm.callWithArgs(DestTypes.DoNotStore, 0, vm.state.pc, vm.state.fp, functionAddr, c);
    }

    @Override public void streamNum(int num, int pos) {
        String numberString = String.format("%d", num);
        if (pos == 0) {
            vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0, vm.state.pc, vm.state.fp);
        }
    
        if (pos >= numberString.length()) {
            int fpVal    = vm.state.popInt();
            int pcVal    = vm.state.popInt();
            int destAddr = vm.state.popInt();
            int destType = vm.state.popInt();
            if (destType == DestTypes.ResumeExecuteFunction) {
                vm.state.pc = pcVal;
                vm.state.fp = fpVal;
            } else {
                throw new IllegalStateException(String.format("FALLBACK, SHOULD NOT HAPPEN, destType is: %d",
                                                              destType));
            }
        } else {
            vm.state.pushCallStub(DestTypes.ResumePrintDecimal, pos + 1, num, vm.state.fp);
            vm.callWithArgsNoCallStub(functionAddr, (int) numberString.charAt(pos));
        }
    }

    @Override public StreamStrState handleResumeUniString(int addr) {
        int currentChar = vm.state.memIntAt(addr);
        if (currentChar != 0) {
            vm.state.pushCallStub(DestTypes.ResumePrintUnicode, 0, addr + 4, vm.state.fp);
            vm.callWithArgsNoCallStub(functionAddr, currentChar);
            return StreamStrState.CallFunction;
        } else {
            return StreamStrState.Finished;
        }
    }

    @Override public StreamStrState handleResumeCString(int addr) {
        int currentChar = vm.state.memByteAt(addr);
        if (currentChar != 0) {
            vm.state.pushCallStub(DestTypes.ResumePrintCString, 0, addr + 1, vm.state.fp);
            vm.callWithArgsNoCallStub(functionAddr, currentChar);
            return StreamStrState.CallFunction;
        } else {
            return StreamStrState.Finished;
        }
    }
  
    // streamstr actions
    private void suspendPrintCompressed(boolean inBetween,
                                        int currentStreamByte,
                                        int currentStreamBit) {
        if (!inBetween) {
            vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                                  vm.state.pc, vm.state.fp);
        }
        vm.state.pushCallStub(DestTypes.ResumePrintCompressed, currentStreamBit,
                              currentStreamByte, vm.state.fp);
    }
  
    public StreamStrState handleChar8(char c, boolean inBetween, int currentStreamByte,
                                      int currentStreamBit) {
        suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit);
        vm.callWithArgsNoCallStub(functionAddr, (int) c);
        return StreamStrState.CallFunction;
    }

    public StreamStrState handleChar32(int c, boolean inBetween, int currentStreamByte,
                                       int currentStreamBit) {
        suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit);
        vm.callWithArgsNoCallStub(functionAddr, c);
        return StreamStrState.CallFunction;
    }

    public StreamStrState handleHuffmanCString(int nodeAddr,
                                               int currentStreamByte,
                                               int currentStreamBit,
                                               boolean inBetween) {
        suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit);
        return StreamStrState.resumeCStringAt(nodeAddr + 1);
    }

    public StreamStrState handleHuffmanUnicodeString(int nodeAddr,
                                                     int currentStreamByte,
                                                     int currentStreamBit,
                                                     boolean inBetween) {
        suspendPrintCompressed(inBetween, currentStreamByte, currentStreamBit);
        return StreamStrState.resumeUniStringAt(nodeAddr + 1);
    }

    @Override public StreamStrState handleStreamstrCString(boolean inBetween,
                                                           int stringAddress) {
        if (!inBetween) {
            vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                                  vm.state.pc, vm.state.fp);
        }
        return StreamStrState.resumeCStringAt(stringAddress + 1);
    }

    @Override public StreamStrState handleStreamstrUnicodeString(boolean inBetween,
                                                                 int stringAddress) {
        if (!inBetween) {
            vm.state.pushCallStub(DestTypes.ResumeExecuteFunction, 0,
                                  vm.state.pc, vm.state.fp);
        }
        return StreamStrState.resumeUniStringAt(stringAddress + 4);
    }
}
