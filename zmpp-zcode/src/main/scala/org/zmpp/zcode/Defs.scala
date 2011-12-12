/*
 * Created on 2010/05/12
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

import scala.annotation.switch
import org.zmpp.base.{VMRunStates, Memory, IntStack}
import org.zmpp.iff.QuetzalCompression

class VMStateImpl extends VMState {
  import QuetzalCompression._

  private var _story : Memory = null
  private val _stack = new IntStack

  var header     : StoryHeader   = null
  val encoding = new ZsciiEncoding(this)
  var runState = VMRunStates.Running
  var calculatedChecksum = 0
  // store the original dynamic memory as a reference point for
  // restart, undo snapshots and saving
  var originalDynamicMem: Array[Byte] = null

  var _pc       = 0
  var fp       = 0 // frame pointer
  def sp       = _stack.sp
  def stack    = _stack

  def storyData = _story.buffer
  def pc = _pc
  def setPC(newpc: Int) = _pc = newpc
  def incrementPC(increment: Int) = _pc += increment

  def reset {
    _stack.setSp(0)
    // Set the initial frame pointer to -1. This is serving as a marker
    // when we search the stack to save
    fp        =  -1
    if (header.version != 6) {
      _pc = header.startPC      
    } else {
      // V6 does function call to main routine
      call(header.startPC, null, -1, 0)
    }
    encoding.reset
    // set interpreter information
    setByteAt(0x1e, 0x06)
    setByteAt(0x1f, '6'.asInstanceOf[Int])
    setShortAt(0x32, 0x0101)    
  }

  def reset(story: Memory) {
    _story = story
    header    = new StoryHeader(_story)
    saveOriginalDynamicMem

    // calculate the checksum before we make any in-memory modifications
    // but after we have a header
    calculatedChecksum = calculateChecksum
    reset
  }

  private def saveOriginalDynamicMem {
    val dynamicMemSize = header.staticStart
    originalDynamicMem = new Array[Byte](dynamicMemSize)
    System.arraycopy(storyData, 0, originalDynamicMem, 0, dynamicMemSize)
  }
  def restoreOriginalDynamicMem {
    System.arraycopy(originalDynamicMem, 0, storyData, 0, header.staticStart)
  }

  private def calculateChecksum = {
    var currentByteAddress = 0x40
    var checksum = 0
    printf("CALC checksum, file size: %d, stored file size: %d\n",
           _story.size, header.fileLength)
    while (currentByteAddress < header.fileLength) {
      checksum += byteAt(currentByteAddress)
      currentByteAddress += 1
    }
    checksum & 0xffff
  }

  def byteAt(addr: Int)  = _story.byteAt(addr)
  def shortAt(addr: Int) = _story.shortAt(addr)
  def intAt(addr: Int)   = _story.intAt(addr)
  def setByteAt(addr: Int, value: Int)  = {
    if (addr >= header.staticStart) {
      throw new IllegalArgumentException("Attempt to write to static memory.")
    }
    _story.setByteAt(addr, value)
  }
  def setShortAt(addr: Int, value: Int) {
    if (addr >= header.staticStart) {
      throw new IllegalArgumentException("Attempt to write to static memory.")
    }
    _story.setShortAt(addr, value)
  }
  def setIntAt(addr: Int, value: Int)   = {
    if (addr >= header.staticStart) {
      throw new IllegalArgumentException("Attempt to write to static memory.")
    }
    _story.setIntAt(addr, value)
  }
  
  def nextByte = {
    _pc += 1
    _story.byteAt(pc - 1)
  }
  def nextShort = {
    _pc += 2
    _story.shortAt(pc - 2)
  }
  def stackEmpty = _stack.sp == 0
  def stackTop = _stack.top
  def replaceStackTopWith(value: Int) = _stack.replaceTopWith(value)
  def pushUserStack(userStack: Int, value: Int): Boolean = {
    val capacity = _story.shortAt(userStack)
    if (capacity == 0) false
    else {
      _story.setShortAt(userStack + capacity * 2, value)
      _story.setShortAt(userStack, capacity - 1)
      true
    }
  }
  def popUserStack(userStack: Int): Int = {
    val capacity = _story.shortAt(userStack)
    _story.setShortAt(userStack, capacity + 1)
    _story.shortAt(userStack + (capacity + 1) * 2)
  }

  def variableValue(varnum: Int) = {
    if (varnum == 0) _stack.pop
    else if (varnum >= 1 && varnum <= 15) { // local
      _stack.valueAt(fp + FrameOffset.Locals + (varnum - 1))
    } else { // global
      _story.shortAt(header.globalVars + ((varnum - 0x10) << 1))
    }
  }
  def setVariableValue(varnum: Int, value: Int) {
    if (varnum == 0) _stack.push(value)
    else if (varnum >= 1 && varnum <= 15) { // local
      //printf("SET L%02x TO %02x\n", varnum, value)
      _stack.setValueAt(fp + FrameOffset.Locals + (varnum - 1), value)
    } else if (varnum >= 16 && varnum <= 255) { // global
      _story.setShortAt(header.globalVars + ((varnum - 0x10) << 1), value)
    }
    // => throw away varnums < 0
  }

  def nextOperand(operandType: Int) = {
    (operandType: @switch) match {
      case 0 => // large
        _pc += 2
        _story.shortAt(pc - 2)
      case 1 => // small
        _pc += 1
        _story.byteAt(pc - 1)
      case 2 => // var
        _pc += 1
        variableValue(_story.byteAt(pc - 1))
    }
  }

  def call(packedAddr: Int, args: Array[Int], storeVar: Int, numArgs: Int) {
    if (packedAddr == 0) setVariableValue(storeVar, 0)
    else {
      val routineAddr = header.unpackRoutineAddress(packedAddr)
      val numLocals = _story.byteAt(routineAddr)
    
      // create a call frame
      val oldfp = fp
      fp = sp // current frame pointer
      _stack.push(pc) // return address
      _stack.push(oldfp)
      _stack.push(storeVar)
      _stack.push(numArgs)
      _stack.push(numLocals)
      _pc = routineAddr + 1 // place PC after routine header

      var i = 0
      if (header.version <= 4) {
        while (i < numLocals) {
          _stack.push(nextShort)
          i += 1
        }
      } else {
        while (i < numLocals) {
          _stack.push(0)
          i += 1
        }
      }
      // set arguments to locals, throw away excess parameters (6.4.4.1)
      val numParams = if (numArgs <= numLocals) numArgs else numLocals
      i = 0
      while (i < numParams) {
        _stack.setValueAt(fp + FrameOffset.Locals + i, args(i))
        i += 1
      }
    }
  }
  def returnFromRoutine(retval: Int) {
    val retpc    = _stack.value32At(fp + FrameOffset.ReturnPC)
    val oldfp    = _stack.valueAt(fp + FrameOffset.OldFP)
    val storeVar = _stack.valueAt(fp + FrameOffset.StoreVar)
    _stack.setSp(fp)
    fp = oldfp
    _pc = retpc
    setVariableValue(storeVar, retval)
  }

  def unwindStackToFramePointer(targetFramePointer: Int) {
    while (fp != targetFramePointer) {
      val oldfp = _stack.valueAt(fp + FrameOffset.OldFP)
      _stack.setSp(fp)
      fp = oldfp
    }
  }

  def numArgsCurrentRoutine = _stack.valueAt(fp + FrameOffset.NumArgs)
  def createSnapshot : Snapshot = {
    new Snapshot(compressDiffBytes(storyData, originalDynamicMem,
                                   header.staticStart),
                 _stack.cloneValues, pc, fp)
  }

  def readSnapshot(snapshot: Snapshot) {
    decompressDiffBytes(snapshot.compressedDiff, originalDynamicMem,
                        storyData, header.staticStart)
    _stack.initFromArray(snapshot.stackValues)
    _pc = snapshot.pc
    fp = snapshot.fp
  }

  def setCapabilityFlags(flags: List[CapabilityFlag]) {
    import CapabilityFlag._

    var flags1 = byteAt(0x01)
    var flags2 = byteAt(0x10)
    flags.foreach(flag => flag match {
      case SupportsColors      => if (header.version >= 5) flags1 |= 0x01
      case SupportsPictures    => if (header.version == 6) flags1 |= 0x02
      case SupportsBoldFont    => if (header.version >= 4) flags1 |= 0x04
      case SupportsItalicFont  => if (header.version >= 4) flags1 |= 0x08
      case SupportsFixedFont   => if (header.version >= 4) flags1 |= 0x10
      case SupportsScreenSplit => if (header.version != 6) flags1 |= 0x20
      case SupportsSound       => if (header.version == 6) flags1 |= 0x20
      case SupportsTimedInput  => if (header.version >= 4) flags1 |= 0x80
      case _ => // do nothing
    })
    setByteAt(0x01, flags1)
    setByteAt(0x10, flags2)
  }


  def doBranch(branchOffset: Int) {
    if (branchOffset == 0)      returnFromRoutine(0)
    else if (branchOffset == 1) returnFromRoutine(1)
    else                        _pc += branchOffset - 2
  }

  def storeResult(result: Int) {
    setVariableValue(nextByte, result)
  }
}
