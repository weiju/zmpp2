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

/*
trait OutputStream {
  def putChar(c: Char)
}

trait InputStream {
  def readChar: Char
}
*/
trait OutputStream {
  def putChar(c: Char)
  //def printNum(num: Int)
  def flush
}

trait InputStream {
  def readLine: Int
}

trait PlatformIO {
  def screenOutputStream : OutputStream
  def keyboardStream     : InputStream
}

/*
class MemoryOutputStream extends OutputStream {
}*/

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
class IoSystem {
  private val outputStreams = new Array[OutputStream](3)
  private val inputStreams  = new Array[InputStream](2)
  private var _currentOutputStreamId    = 0
  private var _currentInputStreamId     = 0
  private var _platformIO: PlatformIO = null

  def reset(platformIO: PlatformIO) {
    outputStreams(0)       = platformIO.screenOutputStream
    inputStreams(0)        = platformIO.keyboardStream
    _currentOutputStreamId = 0
    _currentInputStreamId  = 0
  }
  def currentOutputStream = outputStreams(_currentOutputStreamId)
  def currentOutputStreamId = _currentOutputStreamId + 1
  def currentOutputStreamId_=(streamId: Int) {
    if (streamId < 1 || streamId > 4) {
      printError("Can't set current output stream to id: %d " +
                 "(Only 1-4 are allowed)".format(streamId))
    } else _currentOutputStreamId = streamId - 1
  }

  def currentInputStreamId = _currentInputStreamId
  def currentInputStreamId_=(streamId: Int) {
    if (streamId != 0 && streamId != 1) {
      printError("Can't set current input stream to id: %d " +
                 "(only 0 and 1 are allowed).".format(streamId))
    } else _currentOutputStreamId = streamId - 1
  }

  def printError(msg: String) {
    printf("ERROR: %s", msg)
  }
  def putChar(c: Char) {
    outputStreams(_currentOutputStreamId).putChar(c)
  }
  def printNum(num: Int) {
    val numString = "%d".format(num)
    for (c <- numString) putChar(c)
  }
  def flush {
    outputStreams(_currentOutputStreamId).flush
  }
}
