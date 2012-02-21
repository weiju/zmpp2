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

import java.io.*;
import java.util.List;
import java.util.ArrayList;

public class GlkFileSystem {

    private int _nextId = 1;
    private List<FileReference> _fileRefs = new ArrayList<FileReference>();
  
    private FileReference fileRefWithId(int id) {
        for (FileReference ref: _fileRefs) {
            if (ref.id == id) return ref;
        }
        return null;
    }

    private int addFileRef(int usage, int fmode, File file, int rock) {
        FileReference fileref = new FileReference(_nextId++, usage, fmode, file, rock);
        _fileRefs.add(fileref);
        return fileref.id;
    }
  
    public int createFileRefByName(int usage, String name, int rock) {
        return addFileRef(usage, 0, new File(name), rock);
    }

    public int createFileRefByFile(int usage, int fmode, File file, int rock) {
        return addFileRef(usage, fmode, file, rock);
    }

    public int createFromFileRef(int usage, int fileRefId, int rock) {
        FileReference refFileRef = fileRefWithId(fileRefId);
        return addFileRef(usage, refFileRef.fmode, refFileRef.file, rock);
    }

    public int createTemp(int usage, int rock) {
        try {
            return addFileRef(usage, 0, File.createTempFile("zmpp-glk", "tmp"), rock);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public void deleteFile(int fileRefId) {
        FileReference fileRef = fileRefWithId(fileRefId);
        if (fileRef != null && fileRef.file.exists()) {
            fileRef.file.delete();
        }
    }

    public void destroy(int fileRefId) {
        _fileRefs.remove(fileRefWithId(fileRefId));
    }

    public int getRockForFileRef(int fileRefId) {
        return fileRefWithId(fileRefId).rock;
    }
  
    public FileReference iterate(int id) {
        if (id == 0) return _fileRefs.isEmpty() ? null : _fileRefs.get(0);
        else {
            int i = 0;
            for (; i < _fileRefs.size(); i++) {
                if (_fileRefs.get(i).id == id) break;
            }
            if (i >= _fileRefs.size() - 1) return null;
            return _fileRefs.get(i + 1);
        }
    }
  
    public boolean doesFileExist(int fileRefId) { return fileRefWithId(fileRefId).exists(); }
  
    public GlkStream openFile(int fileRefId, int fmode, int rock) {
        try {
            return new GlkFileStream(fileRefWithId(fileRefId), fmode, rock, false);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public GlkStream openFileUni(int fileRefId, int fmode, int rock) {
        throw new UnsupportedOperationException("Unicode files not supported yet");
    }
}
