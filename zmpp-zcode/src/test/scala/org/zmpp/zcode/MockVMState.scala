/*
 * Created on 2011/10/05
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

import org.zmpp.base.Memory

class MockVMState(storyVersion: Int, dataBytes: Array[Byte]) extends VMState with Memory {
  def buffer: Array[Byte] = null
  def size: Int = 0
  val header = new StoryHeader(this) {
    override def version = storyVersion
  }
  var encoding: ZsciiEncoding = new ZsciiEncoding(this)
  var runState = 0
  var pc = 0
  encoding.resetVMState
  def story = this
  def byteAt(addr: Int) = dataBytes(addr)
  def shortAt(addr: Int) = {
    if (addr == 0x34) 0
    else if (addr == 0x36) 0
    else {
      throw new UnsupportedOperationException
    }
  }
  def intAt(addr: Int) = throw new UnsupportedOperationException()
  def setByteAt(addr: Int, value: Int) {}
  def setShortAt(addr: Int, value: Int) {}
  def setIntAt(addr: Int, value: Int) {}
  def copyBytesTo(dest: Array[Byte], srcOffest: Int, numBytes: Int) {}
  def copyBytesTo(dstOffset: Int, srcOffest: Int, numBytes: Int) {}
  def copyBytesFrom(src: Array[Byte], srcOffset: Int, destOffset: Int, numBytes: Int) {}
}
