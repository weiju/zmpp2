/*
 * Created on 2010/04/01
 * Copyright (c) 2010-2014, Wei-ju Wu.
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
package org.zmpp.base

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.util.logging._

/**
 * Memory implemented with a ByteBuffer object
 * All byte and short are considered to be unsigned values, int's are signed
 * to avoid extension to long values where in most cases, we won't use the full
 * positive range of a 32-bit int
 */
class DefaultMemory(_buffer:Array[Byte],
                    val address:Int, val size:Int,
                    val startIndex: Int)
extends Memory with Comparable[DefaultMemory] {
  def this(arr: Array[Byte]) = this(arr, 0, arr.length, 0)
  def this(arr: Array[Byte], addr: Int) = this(arr, addr, arr.length, 0)

  private def indexForAddress(addr: Int) = addr - address + startIndex

  def buffer = _buffer
  def byteAt    (addr: Int): Int = {
    _buffer(indexForAddress(addr)) & 0xff
  }
  def setByteAt (addr: Int, value: Int) {
    _buffer(indexForAddress(addr)) = (value & 0xff).asInstanceOf[Byte]
  }
  def shortAt   (addr: Int): Int = {
    val offset = indexForAddress(addr)
    ((_buffer(offset) & 0xff) << 8) | (_buffer(offset + 1) & 0xff)
  }
  def setShortAt(addr: Int, value: Int) {
    val offset = indexForAddress(addr)
    _buffer(offset)     = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _buffer(offset + 1) = (value & 0xff).asInstanceOf[Byte]
  }
  def intAt     (addr: Int): Int = {
    val offset = indexForAddress(addr)
    ((_buffer(offset) & 0xff) << 24) | ((_buffer(offset + 1) & 0xff) << 16) |
    ((_buffer(offset + 2) & 0xff) << 8) | (_buffer(offset + 3) & 0xff)    
  }
  def setIntAt  (addr: Int, value: Int) {
    val offset = indexForAddress(addr)
    _buffer(offset)     = ((value >>> 24) & 0xff).asInstanceOf[Byte]
    _buffer(offset + 1) = ((value >>> 16) & 0xff).asInstanceOf[Byte]
    _buffer(offset + 2) = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _buffer(offset + 3) = (value & 0xff).asInstanceOf[Byte]
  }

  def copyBytesTo(dest: Array[Byte], srcOffset: Int, numBytes: Int) {
    if (dest.length < numBytes)
      throw new IllegalArgumentException("data array size too small")
    System.arraycopy(buffer, indexForAddress(srcOffset), dest, 0, numBytes)
  }
  def copyBytesTo(dstOffset: Int, srcOffset: Int, numBytes: Int) {
    val srcIndex  = indexForAddress(srcOffset)
    val destIndex = indexForAddress(dstOffset)
    if ((buffer.length < destIndex + numBytes) ||
        (buffer.length < srcIndex  + numBytes))
      throw new IllegalArgumentException("copying over bounds")
    System.arraycopy(buffer, srcIndex, buffer, destIndex, numBytes)
  }
  def copyBytesFrom(src: Array[Byte], srcOffset: Int, destOffset: Int,
                    numBytes: Int) {
    System.arraycopy(src, srcOffset, buffer, indexForAddress(destOffset), numBytes)
  }

  def containsAddress(addr: Int): Boolean =
    addr >= address && addr < address + size
  def compareTo(other: DefaultMemory) = address - other.address

  def littleEndian = {
    // this currently does not do anything, but it should. The TADS interpreter
    // uses this. Instead, there should be a specialized Little Endian Memory class.
    this
  }
}

object DefaultMemory {
  def create(address: Int, size: Int) =
    new DefaultMemory(new Array[Byte](size), address, size, 0)
}

class MemoryInputStream(_mem: Memory, offset: Int, val size: Int)
extends java.io.InputStream {
  val logger = Logger.getLogger("glulx")
  private var position = 0
  private var mark     = 0

  def this(memory: DefaultMemory) = this(memory, memory.address, memory.size)
  def read: Int = {
    if (position >= size) -1
    else {
      val c = _mem.byteAt(position + offset)
      position += 1
      c
    }
  }
  override def mark(readLimit: Int) {
    mark = position
  }
  override def reset {
    position = mark
  }
}

// Since memory access is one of the most frequent operations, it makes sense
// to have a 0 based memory class, that doesn't need to calculate indexes.
class DefaultMemory0(_buffer:Array[Byte]) extends Memory {

  def size = _buffer.length
  def buffer = _buffer
  def byteAt(addr: Int): Int = _buffer(addr) & 0xff
  def setByteAt(addr: Int, value: Int) {
    _buffer(addr) = (value & 0xff).asInstanceOf[Byte]
  }
  def shortAt(addr: Int): Int = {
    ((_buffer(addr) & 0xff) << 8) | (_buffer(addr + 1) & 0xff)
  }
  def setShortAt(addr: Int, value: Int) {
    _buffer(addr)     = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _buffer(addr + 1) = (value & 0xff).asInstanceOf[Byte]
  }
  def intAt     (addr: Int): Int = {
    ((_buffer(addr) & 0xff) << 24) | ((_buffer(addr + 1) & 0xff) << 16) |
    ((_buffer(addr + 2) & 0xff) << 8) | (_buffer(addr + 3) & 0xff)    
  }
  def setIntAt  (addr: Int, value: Int) {
    _buffer(addr)     = ((value >>> 24) & 0xff).asInstanceOf[Byte]
    _buffer(addr + 1) = ((value >>> 16) & 0xff).asInstanceOf[Byte]
    _buffer(addr + 2) = ((value >>> 8) & 0xff).asInstanceOf[Byte]
    _buffer(addr + 3) = (value & 0xff).asInstanceOf[Byte]
  }

  def copyBytesTo(dest: Array[Byte], srcOffset: Int, numBytes: Int) {
    if (dest.length < numBytes)
      throw new IllegalArgumentException("data array size too small")
    System.arraycopy(_buffer, srcOffset, dest, 0, numBytes)
  }
  def copyBytesTo(dstOffset: Int, srcOffset: Int, numBytes: Int) {
    if ((_buffer.length < dstOffset + numBytes) ||
        (_buffer.length < srcOffset + numBytes))
      throw new IllegalArgumentException("copying over bounds")
    System.arraycopy(_buffer, srcOffset, _buffer, dstOffset, numBytes)
  }
  def copyBytesFrom(src: Array[Byte], srcOffset: Int, dstOffset: Int,
                    numBytes: Int) {
    System.arraycopy(src, srcOffset, _buffer, dstOffset, numBytes)
  }

  def containsAddress(addr: Int): Boolean = addr >= 0 && addr < _buffer.length
}
