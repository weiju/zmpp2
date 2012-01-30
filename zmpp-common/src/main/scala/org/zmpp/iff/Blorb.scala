/*
 * Created on 2010/04/17
 * Copyright (c) 2010, Wei-ju Wu.
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
package org.zmpp.iff

import java.io.{InputStream, IOException}
import org.zmpp.base._

object UsageTypes {
  val Pict = 0x50696374
  val Snd  = 0x536e6420
  val Exec = 0x45786563
}

object ResourceTypes {
  val Picture = 1
  val Sound   = 2
  val Exec    = 3
}

class ResourceInfo(val usage: Int, val number: Int, val start: Int) {
  val resourceType  = usage match {
    case UsageTypes.Pict => ResourceTypes.Picture
    case UsageTypes.Snd  => ResourceTypes.Sound
    case UsageTypes.Exec => ResourceTypes.Exec
    case _               =>
      throw new IllegalArgumentException("Unknown usage type: %04x\n".format(
        usage))
  }
  def isPicture = resourceType == ResourceTypes.Picture
  def isSound   = resourceType == ResourceTypes.Sound
  def isExec    = resourceType == ResourceTypes.Exec

  override def toString = {
    "RESOURCE = (%d, %d, %d)".format(resourceType, number, start)
  }
}

object BlorbData {
  val ResourceIndexEntrySize  = 12
  val ResourceIndexEntryStart = 4
  val RIdx = 0x52496478
  val FORM = 0x464f524d
  val IFRS = 0x49465253
  val ZCOD = 0x5a434f44

  private[this] val BufferSize = 1024
  private[this] val HeaderSize = 52
  private[this] var chunkId = 0
  private[this] var chunklen = 0

  // Checking for valid Blorb files
  def isIffFile(dataBytes: Array[Byte]) = intAt(dataBytes, 0) == FORM
  def isBlorbFile(dataBytes: Array[Byte]) = {
    isIffFile(dataBytes) && intAt(dataBytes, 8) == IFRS
  }
  private def intAt(dataBytes: Array[Byte], offset: Int) = {
    ((dataBytes(offset) & 0xff) << 24) | ((dataBytes(offset + 1) & 0xff) << 16) |
    ((dataBytes(offset + 2) & 0xff) << 8) | (dataBytes(offset + 3) & 0xff)
  }
  private def shortAt(dataBytes: Array[Byte], offset: Int) = {
    ((dataBytes(offset + 0) & 0xff) << 8) | (dataBytes(offset + 1) & 0xff)
  }

  // A faster, less memory-consuming version of a Z info scan
  // which can be used to retrieve the information in a batch, possibly
  // on a memory-restricted environment. We just read information
  // on the fly and just use the header information we need
  // Currently, this can read Z-blorbs and plain Z-files
  def quickScanZInfo(inputStream: InputStream): (Int, Int, String, Int) = {
    val buffer = new Array[Byte](BufferSize)
    // read the identifier
    var bytesRead = inputStream.read(buffer, 0, 12)
    var bytesReadSoFar = bytesRead

    if (isBlorbFile(buffer)) {
      val filelen = intAt(buffer, 4)
      chunkId = 0
      chunklen = 0
      while (chunkId != ZCOD && bytesReadSoFar < filelen) {
        readChunkHeader(inputStream, buffer)
        bytesReadSoFar += 8
        if (chunkId != ZCOD) {
          // pad to even
          var bytesToRead = if (chunklen % 2 == 1) chunklen + 1 else chunklen
          while (bytesToRead > 0) {
            val readlen = math.min(bytesToRead, BufferSize)
            bytesRead = inputStream.read(buffer, 0, readlen)
            bytesToRead -= bytesRead
            bytesReadSoFar += bytesRead
          }
        }
      }
      if (bytesReadSoFar < filelen) {
        inputStream.read(buffer, 0, HeaderSize)
        zinfo(buffer)
      } else throw new IOException("no ZCOD chunk found")
    } else {
      // just assume we are a Z-code file
      inputStream.read(buffer, 12, HeaderSize - 12)
      zinfo(buffer)
    }
  }

  private def zinfo(buffer: Array[Byte]) = {
    val version = buffer(0).asInstanceOf[Int]    
    val serialBuffer = new StringBuilder
    for (i <- 0 until 6) {
      serialBuffer.append(buffer(0x12 + i).asInstanceOf[Char])
    }
    val serial = if (version > 1) serialBuffer.toString else null
    (version, shortAt(buffer, 2), serial, shortAt(buffer, 0x1c))
  }

  private def readChunkHeader(inputStream: InputStream, buffer: Array[Byte]) {
    if (inputStream.read(buffer, 0, 8) == 8) {
      chunkId = intAt(buffer, 0)
      chunklen = intAt(buffer, 4)
    } else throw new IOException("error reading Blorb file")
  }
}

// Assumes that the form chunk given as constructor argument is a valid
// blorb file and provides Blorb-specific accesssors
class BlorbData(val formChunk: FormChunk) {

  def hasZcodeChunk   = formChunk.hasSubChunk("ZCOD")
  def zcodeDataShared = formChunk.chunkDataForIdShared("ZCOD")
  def zcodeData       = formChunk.chunkDataForId("ZCOD")

  def glulxData = formChunk.chunkDataForId("GLUL")
  val frontispieceNum =
    if (formChunk.hasSubChunk("Fspc")) formChunk.chunkDataForId("Fspc").intAt(0)
    else -1
  
  val resources: List[ResourceInfo] = {
    val ridxChunk = formChunk.chunkDataForId("RIdx")
    val numResources = ridxChunk.intAt(0)
    var result: List[ResourceInfo] = Nil
    for (i <- 0 until numResources) {
      val entryAddr = BlorbData.ResourceIndexEntryStart +
                      i * BlorbData.ResourceIndexEntrySize
      val entry = new ResourceInfo(ridxChunk.intAt(entryAddr),
                                   ridxChunk.intAt(entryAddr + Types.SizeInt),
                                   ridxChunk.intAt(entryAddr +
                                                   2 * Types.SizeInt))
      result ::= entry
    }
    result.reverse
  }

  private def resourceWithNum(num: Int, resourceType: Int): ResourceInfo = {
    val list = resources.filter(res => res.number == num &&
                                res.resourceType == resourceType)
    if (list.isEmpty) null
    else list.head
  }
  
  def execResource(num: Int): ResourceInfo =
    resourceWithNum(num, ResourceTypes.Exec)
  def soundResource(num: Int): ResourceInfo =
    resourceWithNum(num, ResourceTypes.Sound)
  def pictureResource(num: Int): ResourceInfo =
    resourceWithNum(num, ResourceTypes.Picture)
  
  private def inputStreamForResource(num: Int,
                                     resourceType: Int): InputStream = {
    val resource = resourceWithNum(num, resourceType)
    val chunk = formChunk.chunkAtAddress(resource.start)
    if (resourceType == ResourceTypes.Sound && chunk.id == "FORM") {
      new MemoryInputStream(formChunk.memory, resource.start, chunk.size)
    } else {
      new MemoryInputStream(formChunk.memory,
                            resource.start + Chunk.HeaderLength,
                            chunk.size)
    }
  }
  
  def soundInputStream(soundnum: Int): InputStream = {
    inputStreamForResource(soundnum, ResourceTypes.Sound)
  }

  def pictureInputStream(picnum: Int): InputStream = {
    inputStreamForResource(picnum, ResourceTypes.Picture)
  }

  def listResources {
    for (res <- resources) {
      if (res.resourceType == ResourceTypes.Sound) {        
        val chunk = formChunk.chunkAtAddress(res.start)
      }
    }
  }
}
