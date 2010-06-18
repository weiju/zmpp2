/*
 * Created on 2010/04/24
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
package org.zmpp.glulx

import java.util.logging._
import java.util.List
import java.util.ArrayList
import java.util.Collections

import org.zmpp.base._

/**
 * Implementation of the Glulx Heap. The current implementation works like this:
 * <ol>
 *   <li>
 *     Always allocate from the highest current address and append new blocks
 *     to the end of the memory block list. This ensures that the list is always
 *     sorted in ascending address order, so we can perform binary search on it
 *     This means allocate() is O(1)
 *   </li>
 *   <li>
 *     Lookup and removal are performed using binary search, so these operations
 *     can be done in O(log n)
 *   </li>
 *   <li>
 *     The binary search we use compares an address with the range that
 *     a memory block represents.
 *   </li>
 *  </ol>
 */
class MemoryHeap(val address: Int) extends Memory {
  val logger = Logger.getLogger("glulx")
  private var _memoryBlocks: List[DefaultMemory] = new ArrayList[DefaultMemory]
  private var _highAddress = address
  
  private def compareAddress(addr: Int, memblock: DefaultMemory) = {
    if (addr < memblock.address) -1
    else if (addr >= memblock.address && addr < memblock.address + memblock.size) 0
    else 1
  }

  private def memblockAtRecursive(addr: Int, left: Int, right: Int): DefaultMemory = {
    if (left > right) null
    else {
      val middle = left + (right - left) / 2
      val compVal = compareAddress(addr, _memoryBlocks.get(middle))
      if (compVal == 0) _memoryBlocks.get(middle)
      else if (compVal < 0) memblockAtRecursive(addr, left, middle - 1)
      else memblockAtRecursive(addr, middle + 1, right)
    }
  }

  def buffer: Array[Byte] = throw new UnsupportedOperationException("buffer() not supported")
  def size = 0 // undefined
  def memblockAt(addr: Int): DefaultMemory = {
    val block = memblockAtRecursive(addr, 0, _memoryBlocks.size - 1)
    //printf("SEARCH BLOCK AT ADDR $%02x FOUND: %b\n", addr, block != null)
    block
  }
  
  def maxAddress: Int = _highAddress
  def allocate(size: Int) = {
    val blockAddress = _highAddress
    logger.info("ALLOCATE HEAP MEM WITH SIZE: %d ADDR: $%02x".format(size, blockAddress))
    _highAddress += size
    val block = DefaultMemory.create(blockAddress, size)
    _memoryBlocks.add(block)
    blockAddress
  }
  
  def free(addr: Int) {
    logger.info("FREE HEAP MEM AT ADDR: $%02x".format(addr))
    _memoryBlocks.remove(memblockAt(addr))
  }
  def active: Boolean = _memoryBlocks.size > 0

  // Memory interface, map to allocated memory blocks
  def byteAt    (addr: Int): Int = memblockAt(addr).byteAt(addr)
  def setByteAt (addr: Int, value: Int) = memblockAt(addr).setByteAt(addr, value)
  def shortAt   (addr: Int): Int = memblockAt(addr).shortAt(addr)
  def setShortAt(addr: Int, value: Int) = memblockAt(addr).setShortAt(addr, value)
  def intAt     (addr: Int): Int = memblockAt(addr).intAt(addr)
  def setIntAt  (addr: Int, value: Int) = memblockAt(addr).setIntAt(addr, value)
  
  // copying data
  def copyBytesTo(dest: Array[Byte], srcOffset: Int, numBytes: Int) {
    memblockAt(srcOffset).copyBytesTo(dest, srcOffset, numBytes)
  }
  def copyBytesTo(dstOffset: Int, srcOffset: Int, numBytes: Int) {
    memblockAt(srcOffset).copyBytesTo(dstOffset, srcOffset, numBytes)
  }
  def copyBytesFrom(src: Array[Byte], srcOffset: Int, destOffset: Int, numBytes: Int) {
    memblockAt(destOffset).copyBytesFrom(src, srcOffset, destOffset, numBytes)
  }
}

