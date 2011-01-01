/*
 * Created on 2010/04/01
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

import java.util.logging._
import java.nio.ByteBuffer

/**
 * A simple byte stack based on a ByteBuffer which wraps a byte array.
 * We use the ByteBuffer to do the conversions for us.
 */
class Stack(size : Int) {
  val logger = Logger.getLogger("glulx")

  val _stackArray = new Array[Byte](size)
  val _stack = ByteBuffer.wrap(_stackArray)

  def empty = sp == 0
  var sp = 0
  
  // 8 bit values
  def pushByte(value : Int) {
    _stack.put(sp, value.asInstanceOf[Byte])
    sp += 1
  }
  def topByte : Int = _stack.get(sp - 1).asInstanceOf[Int] & 0xff
  def popByte : Int = {
    sp -= 1
    _stack.get(sp).asInstanceOf[Int] & 0xff
  }
  
  // 16 bit values
  def pushShort(value : Int) {
    _stack.putChar(sp, value.asInstanceOf[Char])
    sp += 2
  }
  def topShort : Int = _stack.getChar(sp - 2)
  def popShort : Int = {
    sp -= 2
    _stack.getChar(sp)
  }

  // 32 bit values
  def pushInt(value : Int) {
    _stack.putInt(sp, value)
    sp += 4
  }
  def topInt : Int = _stack.getInt(sp - 4)
  def popInt : Int = {
    sp -= 4
    _stack.getInt(sp)
  }
  // to access values at any position within the stack
  def setInt(addr : Int, value : Int) {
    _stack.putInt(addr, value)
  }
  def getInt(addr : Int) = _stack.getInt(addr)
  // TODO: Test cases for byte and short
  def setByte(addr : Int, value : Int) {
    _stack.put(addr, (value & 0xff).asInstanceOf[Byte])
  }
  def getByte(addr : Int) = _stack.get(addr) & 0xff
  def setShort(addr : Int, value : Int) {
    _stack.putChar(addr, (value & 0xff).asInstanceOf[Char])
  }
  def getShort(addr : Int) = _stack.getChar(addr)

  def toStringFromTo(start: Int, end : Int) = {
    val builder = new StringBuilder
    builder.append("(Stack [" + start + "-" + end + ")) = [")
    for (i <- start until end) {
      if (i > start) builder.append(", ")
      builder.append("%02x".format(_stackArray(i)))
    }
    builder.append("]")
    builder.toString
  }

  def toStringFrom(start: Int) = toStringFromTo(start, sp)
  
  def toStringAsIntFrom(start: Int) = {
    val builder = new StringBuilder
    builder.append("(Stack [" + start + "-" + sp + ")) = [")
    var i = start
    while (i < sp) {
      if (i > start) builder.append(", ")
      val intValue = (_stackArray(i) << 24) | (_stackArray(i + 1) << 16) |
                     (_stackArray(i + 2) << 8) | _stackArray(i + 3)
      builder.append("%02x".format(intValue))
      i += 4
    }
    builder.append("]")
    builder.toString
  }
  
  override def toString = toStringFromTo(0, sp)

  def cloneValues = {
    val values = new Array[Byte](sp)
    System.arraycopy(_stackArray, 0, values, 0, sp)
    values
  }
  
  def initFromByteArray(array: Array[Byte]) {
    logger.info("INITFROMBYTEARRAY, ARRAY SIZE: %d".format(array.length))
    sp = array.length
    System.arraycopy(array, 0, _stackArray, 0, sp)
  }
}

