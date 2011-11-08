/*
 * Created on 2011/11/07
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
package org.zmpp.zcode

import java.io.{ByteArrayOutputStream, DataOutputStream}
import org.zmpp.iff.{QuetzalCompression}

class QuetzalWriter(vmState: VMStateImpl, savePC: Int) {
  import QuetzalCompression._

  val byteOut = new ByteArrayOutputStream
  val out = new DataOutputStream(byteOut)

  def write: Boolean = {
    writeIffHeader
    writeIFhdChunk
    writeCMemChunk
    writeStksChunk
    true
  }

  private def writeIffHeader {
    out.writeBytes("FORM")
    out.writeInt(0) // placeholder for number of data bytes starting at the IFZS chunk
    out.writeBytes("IFZS") // Quetzal ID
  }

  private def writeIFhdChunk {
    out.writeBytes("IFhd")
    out.writeInt(13)
    out.writeShort(vmState.header.releaseNumber)
    out.write(vmState.header.serialNumber)
    out.write(vmState.header.checksum)
    out.writeByte((savePC >>> 16) & 0xff)
    out.writeChar(savePC & 0xffff)
  }

  private def writeCMemChunk {
    val compressed = compressDiffBytes(vmState.storyData, vmState.originalDynamicMem,
                                       vmState.header.staticStart)
    System.out.println("# dynamic bytes: " + vmState.header.staticStart)
    System.out.println("# compressed bytes: " + compressed.length)
    val decompressed = decompressDiffBytes(compressed,
                                           vmState.originalDynamicMem,
                                           vmState.header.staticStart)
    System.out.println("# decompressed bytes: " + decompressed.length)
    out.writeBytes("IFhd")
  }

  private def writeStksChunk {
  }
}
