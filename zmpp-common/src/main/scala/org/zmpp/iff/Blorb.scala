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

import java.io.InputStream
import java.util.logging._
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
      throw new IllegalArgumentException("Unknown usage type: %04x\n".format(usage))
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
}

class BlorbData(val formChunk: FormChunk) {
  val logger = Logger.getLogger("zmppbase")
  
  def zcodeData = formChunk.chunkDataForId("ZCOD")
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
                                   ridxChunk.intAt(entryAddr + 2 * Types.SizeInt))
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
  
  def execResource(num: Int): ResourceInfo = resourceWithNum(num,
                                                             ResourceTypes.Exec)
  def soundResource(num: Int): ResourceInfo = resourceWithNum(num,
                                                              ResourceTypes.Sound)
  def pictureResource(num: Int): ResourceInfo =
    resourceWithNum(num, ResourceTypes.Picture)
  
  private def inputStreamForResource(num: Int, resourceType: Int): InputStream = {
    val resource = resourceWithNum(num, resourceType)
    val chunk = formChunk.chunkAtAddress(resource.start)
    if (resourceType == ResourceTypes.Sound && chunk.id == "FORM") {
      //logger.info("INPUTSTREAM FOR AIFF AT RESNUM: %d".format(num))
      new MemoryInputStream(formChunk.memory, resource.start, chunk.size)
    } else {
      new MemoryInputStream(formChunk.memory, resource.start + Chunk.HeaderLength,
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
      logger.info("res #%d, usage: %02x start: %02x".format(res.number, res.usage,
                                                            res.start))
      if (res.resourceType == ResourceTypes.Sound) {        
        val chunk = formChunk.chunkAtAddress(res.start)
        logger.info("RESNUM: %d SOUND ID: %s".format(res.number, chunk.id))
      }
    }
  }
}
