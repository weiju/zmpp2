/*
 * Created on 2010/07/08
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
package org.zmpp.glulx

import java.util.logging.Logger

import org.zmpp.base._
import org.zmpp.iff._
import org.zmpp.glk._

/**
 * The SaveGameLoader directly writes on the specified GlulxVMState object
 * to avoid unnecessary memory allocation.
 */
class SaveGameLoader(glk: Glk, streamId: Int, vmState: GlulxVMState,
                     originalRam: Array[Byte]) {
  val logger = Logger.getLogger("glulx")
  private def readId: Array[Byte] = {
    val result = new Array[Byte](4)
    for (i <- 0 until 4)
      result(i) = glk.get_char_stream(streamId).asInstanceOf[Byte]
    result
  }
  private def readInt: Int = {
    var result = 0
    for (i <- 0 until 4) {
      val c = glk.get_char_stream(streamId).asInstanceOf[Char]
      result |= c << (8 * (3 - i))
    }
    result
  }
  private def readFileBytes(size: Int): Array[Byte] = {
    glk.stream_set_position(streamId, 0, SeekModes.Start)
    val result = new Array[Byte](size)
    for (i <- 0 until size)
      result(i) = glk.get_char_stream(streamId).asInstanceOf[Byte]
    result
  }
  private def isFormId(id: Array[Byte]) = {
    id(0) == 'F'.asInstanceOf[Byte] &&
    id(1) == 'O'.asInstanceOf[Byte] &&
    id(2) == 'R'.asInstanceOf[Byte] &&
    id(3) == 'M'.asInstanceOf[Byte]
  }
  private def isQuetzalFile(formChunk: FormChunk) = formChunk.subId == "IFZS"
  private def readCMemChunk(cmemChunk: Chunk) {
    logger.info("Compressed memory, CHUNKSIZE = %d".format(cmemChunk.size))
    var ramAddress = 0
    val chunkEnd = cmemChunk.dataStart + cmemChunk.size
    var offset = cmemChunk.dataStart

    // read new memory size and adjust VM state's memory size
    val memsize = cmemChunk.memory.intAt(offset)
    logger.info("NEW MEMSIZE = %d [OFFSET = %d]".format(memsize, offset))
    vmState.memsize = cmemChunk.memory.intAt(offset)
    offset += 4

    while (offset < chunkEnd) {
      val b = cmemChunk.memory.byteAt(offset)
      offset += 1
      if (b == 0) {
        if (offset < chunkEnd) {
          val len = cmemChunk.memory.byteAt(offset) + 1
          offset += 1        
          ramAddress += len
        } else {
          ramAddress += 1
        }
      } else {
        // XOR Delta-Value with initial RAM to get saved state
        vmState.setRamByteAt(ramAddress, originalRam(ramAddress) ^ b)
        ramAddress += 1
      }
    }
  }
  private def readUMemChunk(umemChunk: Chunk) {
    logger.info("Uncompressed memory, SIZE = %d".format(umemChunk.size))
    var offset = umemChunk.dataStart
    var ramAddress = 0
    val chunkEnd = umemChunk.dataStart + umemChunk.size
    vmState.memsize = umemChunk.memory.intAt(offset)
    offset += 4
    while (offset < chunkEnd) {
      vmState.setRamByteAt(ramAddress, umemChunk.memory.byteAt(offset))
      ramAddress += 1
      offset += 1
    }
  }
                      
  private def readStksChunk(stksChunk: Chunk) {
    val stackSize = stksChunk.size
    logger.info("Uncompressed memory, SIZE = %d".format(stackSize))
    val stackValues = new Array[Byte](stackSize)
    stksChunk.memory.copyBytesTo(stackValues, stksChunk.dataStart, stackSize)
    vmState.initStackFromByteArray(stackValues)
  }

  private def readMAllChunk(mallChunk: Chunk) {
    logger.info("MAll Chunk found, SIZE = %d TODO".format(mallChunk.size))
  }

  def loadGame: Boolean = {
    logger.info("LOAD_GAME")
    glk.stream_set_position(streamId, 0, SeekModes.End)
    val fileSize = glk.stream_get_position(streamId)
    val fileBytes = readFileBytes(fileSize)
    val formChunk = new DefaultFormChunk(new DefaultMemory(fileBytes))
    if (isQuetzalFile(formChunk)) {

      // Quetzal file
      // we can ignore IFhd for Glulx games, instead, we might have
      // an MAll chunk
      val cmemChunk = formChunk.subChunk("CMem")
      val umemChunk = formChunk.subChunk("UMem")
      val stksChunk = formChunk.subChunk("Stks")
      val mallChunk = formChunk.subChunk("MAll")
      if (cmemChunk == null && umemChunk == null) {
        logger.severe("NO MEMORY CHUNK FOUND !!")
        return false
      }
      if (stksChunk == null) {
        logger.severe("NO STACK CHUNK FOUND !!")
        return false
      }

      logger.info("1. READING RAM")
      if (cmemChunk != null) readCMemChunk(cmemChunk)
      else if (umemChunk != null) readUMemChunk(umemChunk)
      logger.info("2. READING STACK STATE")
      readStksChunk(stksChunk)
      logger.info("3. READING HEAP")
      if (mallChunk != null) readMAllChunk(mallChunk)
      true
    } else {
      logger.severe("NOT A VALID QUETZAL FILE")
      false
    }
  }
}

// Writes state to Quetzal file. Only writes UMem chunks, CMem is not
// necessary
class SaveGameWriter(glk: Glk, streamId: Int, vmState: GlulxVMState,
                     storeLocation: Operand) {
  private var bytesWritten = 0

  private def writeByteArray(bytes: Array[Byte]) {
    for (i <- 0 until bytes.length) {
      glk.put_char_stream(streamId, (bytes(i) & 0xff).asInstanceOf[Char])
    }
  }
  private def writeInt(value: Int) {
    glk.put_char_stream(streamId, ((value >> 24) & 0xff).asInstanceOf[Char])
    glk.put_char_stream(streamId, ((value >> 16) & 0xff).asInstanceOf[Char])
    glk.put_char_stream(streamId, ((value >>  8) & 0xff).asInstanceOf[Char])
    glk.put_char_stream(streamId, ((value >>  0) & 0xff).asInstanceOf[Char])
  }

  private def writeIffHeader {
    glk.stream_set_position(streamId, 0, SeekModes.Start)
    writeByteArray("FORM".getBytes)
    writeInt(bytesWritten)
    writeByteArray("IFZS".getBytes) // Quetzal ID
    bytesWritten += 4 // FORM and size not counted, data size starts at IFZS
  }
  
  private def writeIFhdChunk {
    writeByteArray("IFhd".getBytes)
    writeInt(128)
    for (i <- 0 until 128) {
      glk.put_char_stream(streamId,
                          vmState.memByteAt(i).asInstanceOf[Char])
    }
    bytesWritten += 128 + 8
  }

  private def writeUMemChunk {
    val ramSize   = vmState.ramSize
    val chunkSize = ramSize + 4
    val destRam   = new Array[Byte](ramSize)
    writeByteArray("UMem".getBytes)
    writeInt(chunkSize)
    writeInt(vmState.memsize)
    for (i <- 0 until ramSize) {
      glk.put_char_stream(streamId,
                          vmState.ramByteAt(i).asInstanceOf[Char])
    }
    bytesWritten += ramSize + 12
  }

  private def writeStksChunk {
    vmState.pushCallStub(storeLocation)
    val stackValues = vmState.cloneStackValues
    val stackSize   = stackValues.length 
    writeByteArray("Stks".getBytes)
    writeInt(stackSize)
    writeByteArray(stackValues)
    bytesWritten += stackSize + 8
  }
  private def writeMAllChunk {
  }
  private def writeIntDChunk {
  }

  def writeGameFile: Boolean = {
    writeIffHeader
    writeIFhdChunk
    writeUMemChunk
    writeStksChunk
    writeIntDChunk
    writeIffHeader // write again to log the file size
    true
  }
}
