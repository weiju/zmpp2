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

class DecodeInfo {
    public int form;
    public int operandCount;
    public int opnum;
    public int opcode;
    public int[] types = new int[8];
    public int numOperands;

    public DecodeInfo(int form, int operandCount, int opnum,
                      int opcode) {
        this.form = form;
        this.operandCount = operandCount;
        this.opnum = opnum;
        this.opcode = opcode;
    }
    @Override public String toString() { return opcodeName(5); }

    public DecodeInfo set(int f, int oc, int opn, int b0) {
        form         = f;
        operandCount = oc;
        opnum        = opn;
        opcode       = b0;
        return this;
    }

    private String formName() {
        switch (form) {
        case Instruction.FormLong: return "Long";
        case Instruction.FormShort: return "Short";
        case Instruction.FormVar: return "Var";
        case Instruction.FormExt: return "Ext";
        default: return "???";
        }
    }

    private String opCount() {
        return (operandCount == Instruction.OperandCountVar) ? "Var" :
            String.format("%dOP", operandCount);
    }

    public boolean isCallVx2() {
        return operandCount == Instruction.OperandCountVar &&
            (opnum == 0x1a || opnum == 0x0c);
    }

    public String opcodeName(int version) {
        switch (operandCount) {
        case 0: return Oc0Op.opcodeName(opnum, version);
        case 1: return Oc1Op.opcodeName(opnum, version);
        case 2: return Oc2Op.opcodeName(opnum, version);
        case Instruction.OperandCountVar: return OcVar.opcodeName(opnum, version);
        case Instruction.OperandCountExtVar: return OcExt.opcodeName(opnum, version);
        default: return "???";
        }
    }
}
