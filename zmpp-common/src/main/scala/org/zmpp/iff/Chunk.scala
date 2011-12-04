/*
 * Created on 2010/04/16
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

import org.zmpp.base._

/**
 * General definition of chunks in the IFF file format.
 * IFF files consist of a FORM chunk with nested sub chunks.
 */
object Chunk {
  val IdLength       = 4
  val SizeWordLength = 4
  val HeaderLength   = IdLength + SizeWordLength
}
trait Chunk {
  def id        : String
  def size      : Int
  def memory    : Memory
  def address   : Int
  def dataStart : Int
}

protected class DefaultChunk(val memory: Memory, val address: Int) extends Chunk {
  protected def idAtOffset(offset: Int) = {
    val builder = new StringBuilder
    for (i <- 0 until Chunk.IdLength)
      builder.append(memory.byteAt(address + offset + i).toChar)
    builder.toString
  }

  val id = idAtOffset(0)
  val dataStart = address + Chunk.HeaderLength
  def size    = memory.intAt(address + Chunk.IdLength)
}

/*
 * Quick way to check for a valid IFF file.
 */
object FormChunk {
  def isIffFile(dataBytes: Array[Byte]) = intValueAt(dataBytes, 0) == 0x464f524d
  def isBlorbFile(dataBytes: Array[Byte]) = {
    isIffFile(dataBytes) && intValueAt(dataBytes, 8) == 0x49465253
  }
  private def intValueAt(dataBytes: Array[Byte], offset: Int) = {
    ((dataBytes(offset) & 0xff) << 24) | ((dataBytes(offset + 1) & 0xff) << 16) |
    ((dataBytes(offset + 2) & 0xff) << 8) | (dataBytes(offset + 3) & 0xff)
  }
}

/*
 * FORM Chunks
 */
trait FormChunk extends Chunk {
  def subId: String
  def hasSubChunk(chunkId: String)      : Boolean
  def chunkDataForId(chunkId: String)   : Memory
  def chunkAtAddress(chunkAddr: Int)    : Chunk
  def chunkDataSizeAtAddress(chunkAddr: Int): Int
}

class DefaultFormChunk(_mem: Memory) extends DefaultChunk(_mem, 0) with FormChunk {
  if (!idAtAddressEquals(0, "FORM")) {
    throw new java.io.IOException("not a valid IFF format (ID was '%s')".format(id))
  }
  private def idAtAddressEquals(address: Int, anId: String) : Boolean = {
    for (i <- 0 until Chunk.IdLength) {
      if (_mem.byteAt(address + i).toInt != anId.charAt(i).toInt) return false
    }
    true
  }

  private def subChunkAddress(chunkId: String): Int = {
    var currentAddr = Chunk.HeaderLength + Chunk.IdLength
    val len = size
    while (currentAddr < len) {
      if (idAtAddressEquals(currentAddr, chunkId)) return currentAddr
      var chunkTotalSize = Chunk.HeaderLength +
                           _mem.intAt(currentAddr + Chunk.IdLength)
      chunkTotalSize += (chunkTotalSize % 2) // pad to even if necessary
      currentAddr += chunkTotalSize
    }
    -1
  }

  /*
   * Public accessors
   */  
  val subId = idAtOffset(Chunk.HeaderLength)
  def hasSubChunk(chunkId: String) = subChunkAddress(chunkId) != -1
  def chunkAtAddress(chunkAddr: Int) = new DefaultChunk(_mem, chunkAddr)
  
  def chunkDataSizeAtAddress(chunkAddr: Int) = {
    chunkAtAddress(chunkAddr).size
  }

  def subChunk(chunkId: String): Chunk = {
    if (hasSubChunk(chunkId)) chunkAtAddress(subChunkAddress(chunkId))
    else null
  }
  def chunkDataForId(chunkId: String): Memory = {
    val subChunk = chunkAtAddress(subChunkAddress(chunkId))
    new DefaultMemory(_mem.buffer, 0, subChunk.size, subChunk.dataStart)
  }
}

