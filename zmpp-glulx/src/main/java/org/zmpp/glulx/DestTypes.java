/*
 * Created on 2011/02/12
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

public class DestTypes {
    public static final int DoNotStore    = 0;
    public static final int Memory        = 1;
    public static final int LocalVariable = 2;
    public static final int Stack         = 3;
    public static final int Ram           = 4;

    // while printing
    public static final int ResumePrintCompressed = 10;
    public static final int ResumeExecuteFunction = 11;
    public static final int ResumePrintDecimal    = 12;
    public static final int ResumePrintCString    = 13;
    public static final int ResumePrintUnicode    = 14;
        
    public static boolean isStringDestType(int destType) { return destType >= 10; }

    public static int fromAddressMode(int addressMode) {
        switch (addressMode) {
        case AddressModes.ConstZero:        return DestTypes.DoNotStore;
        case AddressModes.Address00_FF:     return DestTypes.Memory;
        case AddressModes.Address0000_FFFF: return DestTypes.Memory;
        case AddressModes.AddressAny:       return DestTypes.Memory;
        case AddressModes.Stack:            return DestTypes.Stack;
        case AddressModes.Local00_FF:       return DestTypes.LocalVariable;
        case AddressModes.Local0000_FFFF:   return DestTypes.LocalVariable;
        case AddressModes.LocalAny:         return DestTypes.LocalVariable;
        case AddressModes.Ram00_FF:         return DestTypes.Ram;
        case AddressModes.Ram0000_FFFF:     return DestTypes.Ram;
        case AddressModes.RamAny:           return DestTypes.Ram;
        default:
            throw new IllegalArgumentException(String.format("unknown address mode: %02x", addressMode));
        }
    }
}
