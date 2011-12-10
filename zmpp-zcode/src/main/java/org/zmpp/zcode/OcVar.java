/*
 * Created on 2011/12/09
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

public class OcVar {
    private static final String[] names = {
        "CALL", "STOREW", "STOREB", "PUT_PROP", "SREAD", "PRINT_CHAR",
        "PRINT_NUM", "RANDOM", "PUSH", "PULL", "SPLIT_WINDOW", "SET_WINDOW",
        "CALL_VS2", "ERASE_WINDOW", "ERASE_LINE", "SET_CURSOR", "GET_CURSOR",
        "SET_TEXT_STYLE", "BUFFER_MODE", "OUTPUT_STREAM", "INPUT_STREAM",
        "SOUND_EFFECT", "READ_CHAR", "SCAN_TABLE", "NOT", "CALL_VN",
        "CALL_VN2", "TOKENISE", "ENCODE_TEXT", "COPY_TABLE", "PRINT_TABLE",
        "CHECK_ARG_COUNT"
    };

    public static String opcodeName(int opnum, int version) {
        return names[opnum];
    }
}
