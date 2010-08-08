/*
 * Created on 2010/08/07
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
package org.zmpp.zcode

trait OutputStream {
  def putChar(c: Char)
  def flush
  def select(flag: Boolean)
  def isSelected: Boolean
}

trait InputStream {
  def readLine: Int
}

trait PlatformIO {
  def screenOutputStream : OutputStream
  def keyboardStream     : InputStream
  def screenModel        : ScreenModel
}

class MemoryOutputStream(state: VMState)
extends OutputStream {
  private var tableAddr = 0
  private var selected = false
  private var pos: Int = 0

  def table = tableAddr
  def table_=(addr: Int) {
    if (selected) {
      // nesting should actually be supported
      throw new IllegalStateException("can not change table address while " +
                                      "selected - nesting not supported")
    } else {
      tableAddr = addr
      selected  = true
      pos       = 0
    }
  }
  def putChar(c: Char) {
    state.setByteAt(tableAddr + 2 + pos, c & 0xff)
    pos += 1
  }
  def flush { }
  def select(flag: Boolean) {
    // write num written
    if (!flag) state.setShortAt(tableAddr, pos)
    selected = flag
  }
  def isSelected = selected
}

class NullOutputStream extends OutputStream {
  def putChar(c: Char) { }
  def flush { }
  def select(flag: Boolean) { }
  def isSelected = false
}
class NullInputStream extends InputStream {
  def readLine = 0
}

class StringBuilderOutputStream extends OutputStream {
  val builder = new StringBuilder
  def putChar(c: Char) = builder.append(c)
  def flush { }
  def select(flag: Boolean) { }
  def isSelected = false
  override def toString = builder.toString
}

/**
 * Output Streams
 *
 * 1: Screen
 * 2: Transcript
 * 3: Memory
 * 4: Script file
 *
 * Input Streams
 * 0: Keyboard
 * 1: Script File (stream output stream 4)
 */
class IoSystem(state: VMState) extends OutputStream {
  private val outputStreams = new Array[OutputStream](4)
  private val inputStreams  = new Array[InputStream](2)
  private val NullOut = new NullOutputStream
  private var _currentInputStreamId     = 0
  private var _platformIO: PlatformIO = null

  def reset(platformIO: PlatformIO) {
    outputStreams(0)       = platformIO.screenOutputStream
    outputStreams(1)       = NullOut
    outputStreams(2)       = new MemoryOutputStream(state)
    outputStreams(3)       = NullOut
    inputStreams(0)        = platformIO.keyboardStream
    inputStreams(1)        = new NullInputStream
    _currentInputStreamId  = 0
  }
  def selectOutputStream(streamId: Int, flag: Boolean) {
    if (streamId < 1 || streamId > 4) {
      printError("Can't set current output stream to id: %d " +
                 "(Only 1-4 are allowed)".format(streamId))
    } else {
      outputStreams(streamId - 1).select(flag)
    }
  }
  def createAndSelectMemoryStream(table: Int) {
    outputStreams(2).asInstanceOf[MemoryOutputStream].table = table
  }

  def currentInputStreamId = _currentInputStreamId
  def currentInputStreamId_=(streamId: Int) {
    if (streamId != 0 && streamId != 1) {
      printError("Can't set current input stream to id: %d " +
                 "(only 0 and 1 are allowed).".format(streamId))
    } else _currentInputStreamId = streamId - 1
  }

  // ********************************************************************
  // ***** OUTPUT
  // ********************************************************************
  def printError(msg: String) {
    printf("ERROR: %s", msg)
  }
  def printNum(num: Int) {
    val numString = "%d".format(num)
    for (c <- numString) putChar(c)
  }
  def putChar(c: Char) {
    for (s <- outputStreams) {
      if (s.isSelected) s.putChar(c)
    }
  }
  def flush {
    for (s <- outputStreams) {
      if (s.isSelected) s.flush
    }
  }
  def select(flag: Boolean) { }
  def isSelected = true
}
