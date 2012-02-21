/*
 * Created on 2010/04/09
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
package org.zmpp.glk

import java.util.logging._
import org.zmpp.glk.io.{GlkStream, GlkStreamCloseStruct}
import org.zmpp.base.VMState
import org.zmpp.base.Types

/**
 * Byte-based memory stream
 */
abstract class MemoryStream(val state: VMState, val address: Int, val size: Int,
                            val fmode: Int, val rock: Int) extends GlkStream {
  val logger = Logger.getLogger("glk")
  //logger.info("CREATE MEMORYSTREAM, fmode: %02x size: %d".format(fmode, size))
  private[this] var _id    = 0
  private[this] var _style = 0

  def id = _id
  def setId(anId: Int) { _id = anId }
  def style = _style
  def setStyle(aStyle: Int) { _style = aStyle }

  var writeCount = 0
  var readCount  = 0
  var position   = 0
  // append mode
  if (fmode == FileModes.WriteAppend) seekToEnd
  
  private def positionExceedsSize = position >= size
  protected def indexToPos(index: Int) : Int
  protected def setCurrentChar(value: Int)
  protected def currentChar : Int

  private def seekToEnd {
    position = 0
    var endFound = false
    while (!endFound && position < size) {
      if (currentChar == 0) endFound = true
      else position += 1
    }
  }

  def close {
    //logger.info("Closing memory stream, WRITE COUNT = %d, READ COUNT = %d".format(
    //            writeCount, readCount))
  }
  
  def putCharGeneric(c: Int) {
    if (address != 0 && size > 0 && !positionExceedsSize) {
      setCurrentChar(c)
      position += 1
    }
    writeCount += 1
  }
  def putChar(c: Char)   = putCharGeneric(c.asInstanceOf[Int])
  def putCharUni(c: Int) = putCharGeneric(c)

  def getCharGeneric : Int = {
    if (positionExceedsSize) -1
    else {
      readCount += 1
      val retval = currentChar
      position  += 1
      retval
    }
  }
  def getChar    = {
    val retval = getCharGeneric
    if (retval > 255) 0x3f
    else retval
  }
  def getCharUni = getCharGeneric

  def seek(newpos: Int, seekmode: Int) {
    seekmode match {
      case SeekModes.Start   => position = newpos
      case SeekModes.Current => position += newpos
      case SeekModes.End     =>
        seekToEnd
        position += newpos
      case _                 =>
        throw new IllegalArgumentException("Unknown file seek mode: %d".format(
          seekmode))
    }
  }
  def setHyperlink(linkval: Int) {
    throw new UnsupportedOperationException(
      "setHyperlink not supported on memory stream")
  }
}

class MemoryStream8(state: VMState, address: Int, size: Int, fmode: Int, rock: Int)
extends MemoryStream(state, address, size, fmode, rock) {
  protected def indexToPos(index : Int) = index
  private def bufferAddress = address + position
  protected def setCurrentChar(value: Int) = state.setMemByteAt(bufferAddress,
                                                                value)
  protected def currentChar = state.memByteAt(bufferAddress)

  override def putCharUni(c: Int) {
    if (c >= 255) putChar(0x3f)
    else putChar(c.asInstanceOf[Char])
  }
  override def getCharUni: Int = {
    throw new UnsupportedOperationException(
      "getCharUni not supported on MemoryStream8")
  }
}

class MemoryStream32(state: VMState, address: Int, size: Int, fmode: Int, rock: Int)
extends MemoryStream(state, address, size, fmode, rock) {
  protected def indexToPos(index : Int) = index / Types.SizeInt
  protected def setCurrentChar(value: Int) = state.setMemIntAt(bufferAddress, value)
  protected def currentChar = state.memIntAt(bufferAddress)

  private def bufferIndex(pos : Int) = pos * Types.SizeInt
  private def bufferAddress = address + bufferIndex(position)
}

/*
 * This stream is not actually used, we just have it to emulate Glulxe's
 * behavior of having the game file available as a stream. ZMPP does not
 * open the game through Glk streams.
 */
class DummyStream extends GlkStream {
  private[this] var _id = 0

  def id = _id
  def setId(anId: Int) { _id = anId }
  def rock       = 0
  def style      = 0
  def setStyle(value: Int) { }
  def writeCount = 0
  def position   = 0
  def close {}
  def putChar(c: Char) { }
  def putCharUni(c: Int) {}
  def seek(newpos: Int, seekmode: Int) { }
  def setHyperlink(linkval: Int) { }
  def readCount = 0
  def getChar = {
    throw new UnsupportedOperationException("DummyStream does not support getChar")
  }
  def getCharUni = {
    throw new UnsupportedOperationException(
      "DummyStream does not support getCharUni")
  }
}
