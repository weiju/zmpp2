/*
 * Created on 2012/02/20
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
package org.zmpp.glk.io;

import java.io.File;

public class FileReference {
    public int id;
    public int usage;
    public int fmode;
    public File file;
    public int rock;

    public FileReference(int id, int usage, int fmode, File file, int rock) {
        this.id    = id;
        this.usage = usage;
        this.fmode = fmode;
        this.file  = file;
        this.rock  = rock;
    }

    public boolean isBinaryMode() { return (usage & 0x100) == 0; }
    public boolean isTextMode() { return (usage & 0x100) == 0x100; }
    public int fileType() { return usage & FileUsageTypes.TypeMask; }
    public boolean exists() { return file.exists(); }
    public boolean isReadOnly() { return fmode == FileModes.Read; }
    public boolean isWriteOnly() { return isAppend() || fmode == FileModes.Write; }
    public boolean isReadWrite() { return fmode == FileModes.ReadWrite; }
    public boolean isAppend() { return fmode == FileModes.WriteAppend; }
}
