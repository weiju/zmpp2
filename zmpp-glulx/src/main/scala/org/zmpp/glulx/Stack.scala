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

/**
 * A simple byte stack based on a ByteBuffer which wraps a byte array.
 * We use the ByteBuffer to do the conversions for us.
 */
class Stack(size : Int) {
  private[this] val _stackArray = new Array[Byte](size)
  private[this] var _sp = 0
  def sp = _sp
  def sp_=(newSP: Int) {
    _sp = newSP
  }

  def empty = _sp == 0

  // 8 bit values
  def pushByte(value : Int) {
    _stackArray(_sp) = value.asInstanceOf[Byte]
    _sp += 1
  }
  def topByte : Int = {
    (_stackArray(_sp - 1) & 0xff).asInstanceOf[Int]
  }
  def popByte : Int = {
    _sp -= 1
    (_stackArray(_sp) & 0xff).asInstanceOf[Int]
  }
  
  // 16 bit values
  def pushShort(value : Int) {
    _stackArray(_sp) = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _stackArray(_sp + 1) = (value & 0xff).asInstanceOf[Byte]
    _sp += 2
  }
  def topShort : Int = {
    ((_stackArray(_sp - 2) & 0xff) << 8) | (_stackArray(_sp - 1) & 0xff) 
  }
  def popShort : Int = {
    _sp -= 2
    ((_stackArray(_sp) & 0xff) << 8) | (_stackArray(_sp + 1) & 0xff) 
  }

  // 32 bit values
  def pushInt(value : Int) {
    _stackArray(_sp) = ((value >>> 24) & 0xff).asInstanceOf[Byte]
    _stackArray(_sp + 1) = ((value >>> 16) & 0xff).asInstanceOf[Byte]
    _stackArray(_sp + 2) = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _stackArray(_sp + 3) = (value & 0xff).asInstanceOf[Byte]
    _sp += 4
  }
  def topInt : Int = {
    ((_stackArray(_sp - 4) & 0xff) << 24) | ((_stackArray(_sp - 3) & 0xff) << 16) |
    ((_stackArray(_sp - 2) & 0xff) << 8) | (_stackArray(_sp - 1) & 0xff)
  }
  def popInt : Int = {
    _sp -= 4
    ((_stackArray(_sp) & 0xff) << 24) | ((_stackArray(_sp + 1) & 0xff) << 16) |
    ((_stackArray(_sp + 2) & 0xff) << 8) | (_stackArray(_sp + 3) & 0xff)
  }
  // to access values at any position within the stack
  def setByte(addr : Int, value : Int) {
    _stackArray(addr) = (value & 0xff).asInstanceOf[Byte]
  }
  def getByte(addr : Int) = _stackArray(addr) & 0xff

  def setShort(addr : Int, value : Int) {
    _stackArray(addr) = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _stackArray(addr + 1) = (value & 0xff).asInstanceOf[Byte]
  }
  def getShort(addr : Int) = {
    ((_stackArray(addr) & 0xff) << 8) | (_stackArray(addr + 1) & 0xff)
  }

  def setInt(addr : Int, value : Int) {
    _stackArray(addr) = ((value >>> 24) & 0xff).asInstanceOf[Byte]
    _stackArray(addr + 1) = ((value >>> 16) & 0xff).asInstanceOf[Byte]
    _stackArray(addr + 2) = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _stackArray(addr + 3) = (value & 0xff).asInstanceOf[Byte]
  }
  def getInt(addr : Int) = {
    ((_stackArray(addr) & 0xff) << 24) | ((_stackArray(addr + 1) & 0xff) << 16) |
    ((_stackArray(addr + 2) & 0xff) << 8) | (_stackArray(addr + 3) & 0xff)
  }

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

  def toStringFrom(start: Int) = toStringFromTo(start, _sp)

  def toStringAsIntFrom(start: Int) = {
    val builder = new StringBuilder
    builder.append("(Stack [" + start + "-" + _sp + ")) = [")
    var i = start
    while (i < _sp) {
      if (i > start) builder.append(", ")
      val intValue = (_stackArray(i) << 24) | (_stackArray(i + 1) << 16) |
                     (_stackArray(i + 2) << 8) | _stackArray(i + 3)
      builder.append("%02x".format(intValue))
      i += 4
    }
    builder.append("]")
    builder.toString
  }
  
  override def toString = toStringFromTo(0, _sp)

  def cloneValues = {
    val values = new Array[Byte](_sp)
    System.arraycopy(_stackArray, 0, values, 0, _sp)
    values
  }
  
  def initFromByteArray(array: Array[Byte]) {
    _sp = array.length
    System.arraycopy(array, 0, _stackArray, 0, _sp)
  }
}

