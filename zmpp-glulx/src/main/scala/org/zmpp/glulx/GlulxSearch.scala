/*
 * Created on 2010/04/06
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

/****************************************************************************
 **** Glulx Search Package
 ****************************************************************************/
abstract class GlulxSearch(state: GlulxVMState, keySize: Int, options: Int) {
  val keyIndirect       = (options & 0x01) == 0x01
  val zeroKeyTerminates = (options & 0x02) == 0x02
  val returnIndex       = (options & 0x04) == 0x04

  def search: Int

  protected def truncateKey(aKey: Int) = {
    keySize match {
      case 1 => aKey & 0xff
      case 2 => aKey & 0xffff
      case _ => aKey
    }
  }
}

abstract class GlulxArraySearch(state: GlulxVMState, keySize: Int, start: Int,
                                structSize: Int, keyOffset: Int, options: Int)
extends GlulxSearch(state, keySize, options) {

  def structAddress(index: Int) = start + (index * structSize)
  def keyAddress(index: Int) = structAddress(index) + keyOffset

  def keyAt(index: Int) = {
    val addr = keyAddress(index)
    keySize match {
      case 1 => state.memByteAt(keyAddress(index))
      case 2 => state.memShortAt(keyAddress(index))
      case 4 => state.memIntAt(keyAddress(index))
      case _ =>
        throw new IllegalStateException("illegal key size: " + keySize)
    }
  }
  
  /**
   * If returnIndex is true, returns the index of the result or -1.
   * If returnIndex is false, returns the address of the STRUCT found or 0.
   */
  def makeSearchResult(index: Int) = {
    if (index == -1 && !returnIndex) 0
    else {
      if (returnIndex) index else structAddress(index)
    }
  }
}

class LinearSearch(state: GlulxVMState, key: Int, keySize: Int, start: Int,
                   structSize: Int, numStructs: Int, keyOffset: Int,
                   options: Int)
extends GlulxArraySearch(state, keySize, start, structSize, keyOffset, options) {
  private def keyEqualsAtIndex(compareIndex: Int): Boolean = {
    if (keyIndirect) {
      val addr0 = key
      val addr1 = keyAddress(compareIndex)
      var i = 0
      while (i < keySize) {
        val b0 = state.memByteAt(addr0 + i)
        val b1 = state.memByteAt(addr1 + i)
        if (b0 != b1) return false
        i += 1
      }
      true
    } else {
      val searchKey = truncateKey(key)
      val currentKey = keyAt(compareIndex)
      currentKey == searchKey
    }
  }
  
  private def doesCurrentKeyTerminate(compareIndex: Int): Boolean = {
    if (!zeroKeyTerminates) false
    else if (keyIndirect) {
      val addr = keyAddress(compareIndex)
      var i = 0
      while (i < keySize) {
        val b = state.memByteAt(addr + i)
        if (b != 0) return false
        i += 1
      }
      true      
    } else keyAt(compareIndex) == 0
  }

  def search: Int = {
    var i = 0
    var continue = true
    while (continue) {
      if (keyEqualsAtIndex(i)) return makeSearchResult(i)
      else if (doesCurrentKeyTerminate(i)) return makeSearchResult(-1)
      i += 1
      continue = numStructs == -1 || numStructs >= 0 && i < numStructs
    }
    makeSearchResult(-1)
  }
}

class BinarySearch(state: GlulxVMState, key: Int, keySize: Int, start: Int,
                   structSize: Int, numStructs: Int, keyOffset: Int,
                   options: Int)
extends GlulxArraySearch(state, keySize, start, structSize, keyOffset, options) {

  private def keyCompareAtIndex(compareIndex: Int): Int = {
    if (keyIndirect) {
      // indirect search - this means we need to compare lexicographical
      // in memory on an arbitrary key size
      val addr0 = key
      val addr1 = keyAddress(compareIndex)
      var i = 0
      while (i < keySize) {
        val b0 = state.memByteAt(addr0 + i)
        val b1 = state.memByteAt(addr1 + i)
        if (b0 < b1) return -1
        else if (b0 > b1) return 1
        i += 1
      }
      0
    } else {
      // compare as unsigned long values, to prevent funny effects
      // Java ints are always signed
      val currentKey: Long = keyAt(compareIndex).toLong & 0x0ffffffffl
      val directKey : Long = truncateKey(key).toLong & 0x0ffffffffl
      if      (currentKey == directKey) 0
      else if (directKey < currentKey) -1
      else 1
    }
  }

  private def binsearch(left: Int, right: Int): Int = {
    if (left > right) return makeSearchResult(-1)
    val middle = left + (right - left) / 2
    val comp = keyCompareAtIndex(middle)
    if (comp == 0) return makeSearchResult(middle)
    if (comp < 0)  return binsearch(left, middle - 1)
    else           return binsearch(middle + 1, right)
  }

  def search: Int = {
    if (zeroKeyTerminates)
      throw new UnsupportedOperationException("zeroKeyTerminates not allowed for binary search")
    binsearch(0, numStructs - 1)
  }
}

class LinkedSearch(state: GlulxVMState, key: Int, keySize: Int, start: Int,
                   keyOffset: Int, nextOffset: Int, options: Int)
  extends GlulxSearch(state, keySize, options) {

  def keyAtAddress(addr: Int) = {
    keySize match {
      case 1 => state.memByteAt(addr)
      case 2 => state.memShortAt(addr)
      case 4 => state.memIntAt(addr)
      case _ =>
        throw new IllegalStateException("illegal key size: " + keySize)
    }
  }
  
  def keyCompare(key: Int, currentAddr: Int): Boolean = {
    if (keyIndirect) {
      var i = 0
      while (i < keySize) {
        val b0 = state.memByteAt(key + i)
        val b1 = state.memByteAt(currentAddr + keyOffset + i)
        if (b0 != b1) return false
        i += 1
      }
      true
    }
    else truncateKey(key) == keyAtAddress(currentAddr + keyOffset)
  }
  
  def doesCurrentKeyTerminate(currentAddr: Int): Boolean = {
    if (!zeroKeyTerminates) false
    else if (keyIndirect) {
      var i = 0
      while (i < keySize) {
        if (state.memByteAt(currentAddr + keyOffset + i) != 0) return false
        i += 1
      }
      true
    }
    else keyAtAddress(currentAddr + keyOffset) == 0
  }

  def search: Int = {
    if (returnIndex)
      throw new UnsupportedOperationException("returnIndex not supported for linked search")
    var currentAddr = start
    var pos = 0
    while (currentAddr != 0) {
      if (keyCompare(key, currentAddr)) return currentAddr
      if (doesCurrentKeyTerminate(currentAddr)) return 0
      pos += 1
      currentAddr = state.memIntAt(currentAddr + nextOffset)
    }
    0
  }
}

