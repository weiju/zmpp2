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

import java.io.InputStream;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import org.zmpp.base.*;

// Assumes that the form chunk given as constructor argument is a valid
// blorb file and provides Blorb-specific accesssors
class BlorbData {
    private static final int ResourceIndexEntrySize  = 12;
    private static final int ResourceIndexEntryStart = 4;

    public FormChunk formChunk;
    public int frontispieceNum;
    public List<ResourceInfo> resources = new ArrayList<ResourceInfo>();

    public BlorbData(FormChunk formChunk) {
        this.formChunk = formChunk;
        this.frontispieceNum = (formChunk.hasSubChunk("Fspc")) ?
            formChunk.chunkDataForId("Fspc").intAt(0) : -1;


        Memory ridxChunk = formChunk.chunkDataForId("RIdx");
        int numResources = ridxChunk.intAt(0);
        for (int i = 0; i < numResources; i++) {
            int entryAddr = ResourceIndexEntryStart + i * ResourceIndexEntrySize;
            ResourceInfo entry = new ResourceInfo(ridxChunk.intAt(entryAddr),
                                                  ridxChunk.intAt(entryAddr + Types.SizeInt),
                                                  ridxChunk.intAt(entryAddr +
                                                                  2 * Types.SizeInt));
            resources.add(entry);
        }
    }

    public boolean hasZcodeChunk() { return formChunk.hasSubChunk("ZCOD"); }
    public Memory zcodeDataShared() { return formChunk.chunkDataForIdShared("ZCOD"); }
    public Memory zcodeData() { return formChunk.chunkDataForId("ZCOD"); }
    public Memory glulxData() { return formChunk.chunkDataForId("GLUL"); }

  
    private ResourceInfo resourceWithNum(int num, int resourceType) {
        for (ResourceInfo info : resources) {
            if (info.number == num && info.resourceType == resourceType) return info;
        }
        return null;
    }
  
    public ResourceInfo execResource(int num) {
        return resourceWithNum(num, ResourceTypes.Exec);
    }

    public ResourceInfo soundResource(int num) {
        return resourceWithNum(num, ResourceTypes.Sound);
    }

    public ResourceInfo pictureResource(int num) {
        return resourceWithNum(num, ResourceTypes.Picture);
    }
  
    private InputStream inputStreamForResource(int num, int resourceType) {
        ResourceInfo resource = resourceWithNum(num, resourceType);
        Chunk chunk = formChunk.chunkAtAddress(resource.start);
        if (resourceType == ResourceTypes.Sound && "FORM".equals(chunk.id())) {
            return new MemoryInputStream(formChunk.memory(), resource.start, chunk.size());
        } else {
            return new MemoryInputStream(formChunk.memory(),
                                         resource.start + Chunk.HeaderLength,
                                         chunk.size());
        }
    }
  
    public InputStream soundInputStream(int soundnum) {
        return inputStreamForResource(soundnum, ResourceTypes.Sound);
    }

    public InputStream pictureInputStream(int picnum) {
        return inputStreamForResource(picnum, ResourceTypes.Picture);
    }

    public void listResources() {
        for (ResourceInfo res: resources) {
            if (res.resourceType == ResourceTypes.Sound) {        
                Chunk chunk = formChunk.chunkAtAddress(res.start);
            }
        }
    }
}
