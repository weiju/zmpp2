/*
 * Created on 2011/11/07
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

import scala.collection.JavaConversions._
import java.io.{ByteArrayOutputStream, DataOutputStream}
import org.zmpp.iff.{QuetzalCompression}
import java.util.ArrayList

/**
 * store variable = -1 => ignore
 */
case class StackFrame(pc: Int, storeVariable: Int,
                      numArgs: Int, locals: Array[Int],
                      evalStackWords: Array[Int]) {
  def write(out: DataOutputStream) {
    out.writeByte((pc >>> 16) & 0xff)
    out.writeChar(pc & 0xffff)
    var flags = locals.length
    if (storeVariable == -1) flags |= 0x10
    out.writeByte(flags & 0xff)
    out.writeByte(storeVariable & 0xff)
    out.writeByte(argByte)
    out.writeChar(evalStackWords.length)
    for (i <- 0 until locals.length) out.writeChar(locals(i) & 0xff)
    for (i <- 0 until evalStackWords.length) out.writeChar(evalStackWords(i) & 0xff)

    printf("pc = $%04x, flags = $%02x storeVar = %d, argByte = $%02x, # eval stack = %d, locals = [",
           pc, flags & 0xff, storeVariable, argByte & 0xff, evalStackWords.length)
    for (i <- 0 until locals.length) {
      if (i > 0) printf(", ")
      printf("%d", locals(i))
    }
    printf("], eval stack = [")
    for (i <- 0 until evalStackWords.length) {
      if (i > 0) printf(", ")
      printf("%d", evalStackWords(i))
    }
    printf("]\n")
  }

  // basically 2^n - 1 for n > 0
  def argByte = if (numArgs == 0) 0 else (1 << numArgs) - 1
  def sizeInBytes = 8 + locals.length * 2 + evalStackWords.length * 2
  override def toString = {
    "pc = $%04x, storevar = %d, # args = %d, locals = %s, eval stack = %s".format(
    pc, storeVariable, numArgs, locals, evalStackWords)
  }
}

class QuetzalWriter(vmState: VMStateImpl, savePC: Int) {
  import QuetzalCompression._

  val byteOut = new ByteArrayOutputStream
  val out = new DataOutputStream(byteOut)

  def write: Boolean = {
    writeIffHeader
    var numFormBytes = 8
    numFormBytes += writeIFhdChunk
    numFormBytes += writeCMemChunk
    numFormBytes += writeStksChunk
    out.flush
    out.close
    val dataBytes: Array[Byte] = byteOut.toByteArray
    printf("# form bytes = %d\n", dataBytes.length)
    true
  }

  private def writeIffHeader {
    out.writeBytes("FORM")
    out.writeInt(0) // placeholder for number of data bytes starting at the IFZS chunk
    out.writeBytes("IFZS") // Quetzal ID
  }

  private def writeIFhdChunk = {
    out.writeBytes("IFhd")
    out.writeInt(13)
    out.writeShort(vmState.header.releaseNumber)
    out.write(vmState.header.serialNumber)
    out.writeShort(vmState.header.checksum)
    out.writeByte((savePC >>> 16) & 0xff)
    out.writeChar(savePC & 0xffff)
    out.writeByte(0) // pad byte
    22 // "IFhd" + length + (13 data bytes + pad)
  }

  private def writeCMemChunk = {
    val compressed = compressDiffBytes(vmState.storyData, vmState.originalDynamicMem,
                                       vmState.header.staticStart)
    out.writeBytes("CMem")
    printf("CMem: compressed data len = %d\n", compressed.length)
    out.writeInt(compressed.length)
    out.write(compressed)
    val datalen = if ((compressed.length % 2) == 1) {
      out.writeByte(0)
      compressed.length + 1
    } else compressed.length
    printf("CMem size = %d\n", datalen + 8)
    datalen + 8
  }

  private def writeStksChunk = {
    out.writeBytes("Stks")
    val stackFrames = getStackFrames
    var numStackFrameBytes = 0
    stackFrames.foreach { stackFrame =>
      numStackFrameBytes += stackFrame.sizeInBytes
    }
    printf("# stack frame bytes = %d\n", numStackFrameBytes)
    out.writeInt(numStackFrameBytes)
    for (i <- stackFrames.size - 1 to 0 by - 1) {
      //printf("Stack Frame: %s\n", stackFrames.get(i))
      printf("Stack frame %d: ", i)
      stackFrames.get(i).write(out)
    }
    printf("Stks size = %d\n", numStackFrameBytes + 8)
    numStackFrameBytes + 8
  }

  private def getStackFrames = {
    import FrameOffset._

    // 1. build a list of stack frames working from top to bottom
    val stackFrames = new ArrayList[StackFrame]
    var currentFp = vmState.fp
    var lastValidFp = currentFp
    var currentSp = vmState.sp // top of the stack of the current processed frame
    while ((currentFp & 0xffff) != 0xffff) {
      val numLocals = vmState.stack.valueAt(currentFp + NumLocals)
/*
      printf("CURRENT FP IS: %d, RET PC = $%04x OLDFP = %d, STOREVAR = %d, # args = %d, # locals: %d\n",
             currentFp, vmState.stack.value32At(currentFp + ReturnPC),
             vmState.stack.valueAt(currentFp + OldFP),
             vmState.stack.valueAt(currentFp + StoreVar),
             vmState.stack.valueAt(currentFp + NumArgs),
             numLocals)*/
      val locals = new Array[Int](numLocals)
      for (i <- 0 until numLocals) locals(i) = vmState.stack.valueAt(currentFp + Locals + i)

      val evalStackWordsStart = currentFp + NumInfoWords + numLocals
      val numEvalStackWords = currentSp - evalStackWordsStart
      printf("# eval stack words: %d\n", numEvalStackWords)

      val evalStackWords = new Array[Int](numEvalStackWords)
      for (i <- 0 until numEvalStackWords) {
        evalStackWords(i) = vmState.stack.valueAt(evalStackWordsStart + i)
      }
      val stackFrame = StackFrame(vmState.stack.value32At(currentFp + ReturnPC),
                                  vmState.stack.valueAt(currentFp + StoreVar),
                                  vmState.stack.valueAt(currentFp + NumArgs),
                                  locals, evalStackWords)
      stackFrames.add(stackFrame)
      lastValidFp = currentFp
      currentSp = currentFp
      currentFp = vmState.stack.valueAt(currentFp + OldFP)
    }
    if (vmState.header.version != 6) {
      // push dummy frame, only use only the eval stack words
      val evalStackWords: Array[Int] = new Array[Int](lastValidFp)
      for (i <- 0 until lastValidFp) evalStackWords(i) = vmState.stack.valueAt(i)
      val dummyStackFrame = StackFrame(0, 0, 0, Array(), evalStackWords)
      stackFrames.add(dummyStackFrame)
    }
    stackFrames
  }
}
