/*
 * Created on 2010/04/09
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
package org.zmpp.glk

import java.util.logging._
import org.zmpp.base.VMState
import org.zmpp.base.Types

trait GlkStream {
  def id       : Int
  def id_=(anId: Int)
  def rock     : Int
  def position : Int
  def seek(newpos: Int, seekmode: Int)
  def close
  
  // input methods
  def readCount  : Int
  def getChar    : Int
  def getCharUni : Int

  // output methods
  def writeCount: Int
  def style: Int
  def style_=(value: Int)
  def putChar(c: Char)
  def putCharUni(c: Int)
  def setHyperlink(linkval: Int)
}

class GlkStreamCloseStruct(_writeCount: Int, _readCount: Int) {
  def writeCount = _writeCount
  def readCount = _readCount
}

class GlkIOSystem {
  val logger = Logger.getLogger("glk")
  private var _nextId = 1
  private var _streams: List[GlkStream] = Nil
  private var _currentStream: GlkStream = NilStream

  def streamWithId(id: Int) = {
    _streams.filter(stream => stream.id == id).head
  }

  def registerStream(stream: GlkStream): Int = {
    stream.id = _nextId
    _streams = stream :: _streams
    _nextId += 1
    stream.id
  }
  def iterate(id: Int): GlkStream = {
    if (_streams.isEmpty) null
    else if (id == 0) _streams.head
    else {
      val remain = _streams.dropWhile(stream => stream.id != id).tail
      if (remain.isEmpty) null
      else remain.head 
    }
  }
  def closeStream(id: Int) = {
    val stream = streamWithId(id)
    _streams = _streams.filterNot(stream => stream.id == id)
    stream.close
    if (stream == _currentStream) _currentStream = NilStream
    new GlkStreamCloseStruct(stream.writeCount, stream.readCount)
  }
  def getPosition(id: Int) = streamWithId(id).position
  def setPosition(id: Int, pos: Int, seekmode: Int) = streamWithId(id).seek(pos, seekmode)
  def putChar(id: Int, c: Char) = streamWithId(id).putChar(c)
  def putCharUni(id: Int, c: Int) = streamWithId(id).putCharUni(c)
  
  // control current stream
  def currentStream = _currentStream
  def currentStream_=(s: GlkStream) {
    _currentStream = if (s == null) NilStream else s
  }
  def currentStreamId = _currentStream.id
  def currentStreamId_=(id: Int) = _currentStream = streamWithId(id)

  def getRock(streamId: Int) = streamWithId(streamId).rock

  def putChar(c: Char)   = _currentStream.putChar(c)
  def putCharUni(c: Int) = _currentStream.putCharUni(c)
  // For convenient output (debugging etc.)
  def putJavaString(str: String) = for (c <- str) putCharUni(c)
  def setHyperlink(linkval: Int) = _currentStream.setHyperlink(linkval)
  def setHyperlinkStream(streamId: Int, linkval: Int) {
    streamWithId(streamId).setHyperlink(linkval)
  }
  def currentStyle = _currentStream.style
  def currentStyle_=(value: Int) = _currentStream.style = value
  
  def setStyle(streamId: Int, value: Int) {
    streamWithId(streamId).style = value
  }
  // reading
  def getCharStream(streamId: Int)    = streamWithId(streamId).getChar
  def getCharStreamUni(streamId: Int) = streamWithId(streamId).getCharUni
}

/**
 * Byte-based memory stream
 */
abstract class MemoryOutputStream(_state: VMState, _address: Int, _size: Int,
                                  _mode: Int, val rock: Int) extends GlkStream {
  val logger = Logger.getLogger("glk")
  var id         = 0
  var style      = 0
  var writeCount = 0
  def position   = writeCount // for now
  def close {
    //logger.info(
    //  "CLOSING MEMORY STREAM ID = %d WRITECOUNT = %d".format(id, writeCount))
  }
  def readCount = 0
  def getChar = {
    throw new UnsupportedOperationException("MemoryOutputStream does not support getChar")
  }
  def getCharUni = {
    throw new UnsupportedOperationException("MemoryOutputStream does not support getCharUni")
  }
  def seek(newpos: Int, seekmode: Int) {
    throw new UnsupportedOperationException("MemoryOutputStream.seek() not supported yet")
  }
  def setHyperlink(linkval: Int) {
    throw new UnsupportedOperationException("setHyperlink not supported on memory stream")
  }
}

class MemoryOutputStream8(state: VMState, address: Int, size: Int, mode: Int, rock: Int)
extends MemoryOutputStream(state, address, size, mode, rock) {

  def putChar(c: Char) {
    //println("MemoryStream PRINTCHAR: " + c)
    if (address != 0 && size > 0 && writeCount < size) {
      state.setMemByteAt(address + writeCount, c.asInstanceOf[Int])
    }
    writeCount += 1
  }
  def putCharUni(c: Int) {
    putChar(c.asInstanceOf[Char])
    //throw new UnsupportedOperationException("put_char_uni not allowed on byte memory stream")
  }
}

class MemoryOutputStream32(state: VMState, address: Int, size: Int, mode: Int, rock: Int)
extends MemoryOutputStream(state, address, size, mode, rock) {
  def putChar(c: Char) {
    //println("MemoryStream32 PRINTCHAR: " + c)
    putCharUni(c.toInt)
  }
  def putCharUni(c: Int) {
    //println("MemoryStream32 PRINTCHAR_UNI: " + (c & 0xffff).asInstanceOf[Char])
    if (address != 0 && size > 0 && writeCount < size) {
      state.setMemIntAt(address + writeCount * Types.SizeInt, c)
    }
    writeCount += 1
  }
}

object MemoryStreamFactory {
  def createMemoryStream8(state: VMState, buf: Int, buflen: Int, fmode: Int,
                          rock: Int): GlkStream = {
    if (fmode == FileModes.Read || fmode == FileModes.ReadWrite) {
      throw new UnsupportedOperationException("read and read/write not supported yet for memory streams")
    }
    if (fmode == FileModes.WriteAppend) {
      throw new IllegalArgumentException("illegal file mode for memory stream: WriteAppend")
    }
    new MemoryOutputStream8(state, buf, buflen, fmode, rock)
  }
  def createMemoryStream32(state: VMState, buf: Int, buflen: Int, fmode: Int,
                           rock: Int): GlkStream = {
    if (fmode == FileModes.Read || fmode == FileModes.ReadWrite) {
      throw new UnsupportedOperationException("read and read/write not supported yet for memory streams")
    }
    if (fmode == FileModes.WriteAppend) {
      throw new IllegalArgumentException("illegal file mode for memory stream: WriteAppend")
    }
    new MemoryOutputStream32(state, buf, buflen, fmode, rock)
  }
}

object NilStream extends GlkStream {
  def rock       = 0
  def id         = 0
  def id_=(anId: Int) {}
  def style      = 0
  def style_=(value: Int) { }
  def writeCount = 0
  def position   = 0
  def close {}
  def putChar(c: Char) { }
  def putCharUni(c: Int) { }
  def readCount = 0
  def getChar = {
    throw new UnsupportedOperationException("NilStream does not support getChar")
  }
  def getCharUni = {
    throw new UnsupportedOperationException("NilStream does not support getCharUni")
  }

  def seek(newpos: Int, seekmode: Int) { }
  def setHyperlink(linkval: Int) { }
}

/*
 * This stream is not actually used, we just have it to emulate Glulxe's
 * behavior of having the game file available as a stream. ZMPP does not
 * open the game through Glk streams.
 */
class DummyStream extends GlkStream {
  def rock       = 0
  var id         = 0
  def style      = 0
  def style_=(value: Int) { }
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
    throw new UnsupportedOperationException("DummyStream does not support getCharUni")
  }
}

