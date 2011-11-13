/*
 * Created on 2010/08/07
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

object StreamIds {
  val Screen     = 1
  val Transcript = 2
  val Memory     = 3
  val ScriptOut  = 4

  val Keyboard   = 0
  val ScriptIn   = 1
}

trait OutputStream {
  def putChar(c: Char)
  def flush
  def select(flag: Boolean)
  def isSelected: Boolean
}


trait InputStream {
  def readLine: Int
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
    if (c != 0) {
      state.setByteAt(tableAddr + 2 + pos, c & 0xff)
      pos += 1
    }
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

/**
 * A simple output stream that maps to a string builder. Used for status
 * line rendering in V3.
 */
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
 */
class IoSystem(state: VMState) extends OutputStream {
  import StreamIds._

  // entry 0 is null
  private val outputStreams = new Array[OutputStream](5)
  private val inputStreams  = new Array[InputStream](2)
  private val NullOut = new NullOutputStream
  private var _currentInputStreamId     = Keyboard
  private var _screenModel: ScreenModel = null

  def reset(screenModel: ScreenModel) {
    outputStreams(0)          = NullOut
    outputStreams(Screen)     = screenModel.screenOutputStream
    outputStreams(Transcript) = NullOut
    outputStreams(Memory)     = new MemoryOutputStream(state)
    outputStreams(ScriptOut)  = NullOut
    inputStreams(Keyboard)    = screenModel.keyboardStream
    inputStreams(ScriptIn)    = new NullInputStream
    _currentInputStreamId     = Keyboard
    _screenModel              = screenModel
  }
  def selectOutputStream(streamId: Int, flag: Boolean) {
    if (streamId < Screen || streamId > ScriptOut) {
      printError("Can't set current output stream to id: %d " +
                 "(Only 1-4 are allowed)".format(streamId))
    } else {
      printf("SELECT OUTPUTSTREAM %d: %b\n", streamId, flag)
      outputStreams(streamId).select(flag)
    }
  }
  def createAndSelectMemoryStream(table: Int) {
    outputStreams(Memory).asInstanceOf[MemoryOutputStream].table = table
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
  def printNum(num: Int) = "%d".format(num).map{c => putChar(c)}
  def putChar(c: Char) {
    // if stream 3 is selected, only write to that one
    if (outputStreams(Memory).isSelected) outputStreams(Memory).putChar(c)
    else outputStreams.filter{s => s.isSelected}.map{s => s.putChar(c)}
  }
  def flush = outputStreams.filter{s => s.isSelected}.map{s => s.flush}
  def select(flag: Boolean) { }
  def isSelected = true

  // selective output
  def putChar(c: Char, stream: Int) = outputStreams(stream).putChar(c)
  def flush(stream: Int) = outputStreams(stream).flush
  def printMessage(msg: String) = {
    _screenModel.setWindow(0)
    msg.map(c => putChar(c, Screen))
    flush(Screen)
  }
  def printError(msg: String) = printMessage("ERROR: %s".format(msg))
}
