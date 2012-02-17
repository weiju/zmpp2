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


public class ResourceInfo {
    public static class ResourceTypes {
        public static final int Picture = 1;
        public static final int Sound   = 2;
        public static final int Exec    = 3;
    }
    public static class UsageTypes {
        public static final int Pict = 0x50696374;
        public static final int Snd  = 0x536e6420;
        public static final int Exec = 0x45786563;
    }

    public int usage;
    public int number;
    public int start;
    public int resourceType;

    public ResourceInfo(int usage, int number, int start) {
        this.usage  = usage;
        this.number = number;
        this.start  = start;

        switch (usage) {
        case UsageTypes.Pict: resourceType = ResourceTypes.Picture; break;
        case UsageTypes.Snd:  resourceType = ResourceTypes.Sound; break;
        case UsageTypes.Exec: resourceType = ResourceTypes.Exec; break;
        default:
            throw new IllegalArgumentException(String.format("Unknown usage type: %04x\n",
                                                             usage));
        }
    }

    public boolean isPicture() { return resourceType == ResourceTypes.Picture; }
    public boolean isSound() { return resourceType == ResourceTypes.Sound; }
    public boolean isExec() { return resourceType == ResourceTypes.Exec; }

    @Override public String toString() {
        return String.format("RESOURCE = (%d, %d, %d)", resourceType, number, start);
    }
}
