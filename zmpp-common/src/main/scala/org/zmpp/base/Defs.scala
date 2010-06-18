/*
 * Created on 2010/04/16
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
package org.zmpp.base

/*************************************************************************
 ***** ZMPP base definitions
 *************************************************************************/

object Types {
  val ByteType   = 1
  val ShortType  = 2
  val IntType    = 4
  
  val SizeByte   = 1
  val SizeShort  = 2
  val SizeInt    = 4

  def isValidType(valtype : Int) =
    valtype == ByteType | valtype == ShortType || valtype == IntType
}

object VMRunStates {
  val Halted       = 0
  val Running      = 1
  val WaitForEvent = 2
}

trait Memory {
  def buffer: Array[Byte]
  def size: Int
  def byteAt    (address : Int) : Int
  def setByteAt (address : Int, value : Int)
  def shortAt   (address : Int) : Int
  def setShortAt(address : Int, value : Int)
  def intAt     (address : Int) : Int
  def setIntAt  (address : Int, value : Int)
  
  // copying data
  def copyBytesTo(dest: Array[Byte], srcOffset: Int, numBytes: Int)
  def copyBytesTo(dstOffset: Int, srcOffset: Int, numBytes: Int)
  
  def copyBytesFrom(src: Array[Byte], srcOffset: Int, destOffset: Int, numBytes: Int)
}

trait VMState {
  def runState: Int
  def runState_=(state: Int)

  // memory access
  def memByteAt    (address: Int) : Int
  def setMemByteAt (address: Int, value: Int)
  def memShortAt   (address: Int) : Int
  def setMemShortAt(address: Int, value: Int)
  def memIntAt     (address: Int) : Int
  def setMemIntAt  (address: Int, value: Int)

  // stack access
  def pushByte(value : Int)
  def topByte : Int
  def popByte : Int
  def pushShort(value: Int)
  def topShort : Int
  def popShort : Int
  def pushInt(value : Int)
  def topInt : Int
  def popInt : Int
}

