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
import java.io.{ByteArrayOutputStream, DataOutputStream, FileOutputStream, DataInputStream}
import org.zmpp.iff.{QuetzalCompression}
import java.util.ArrayList

class NonMatchingGameSaveException extends Exception
class IllegalFormatException extends Exception

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
    if ((storeVariable & 0xff) == 0xff) flags |= 0x10
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

object QuetzalFormat {
  val IdFORM = 0x464f524d
  val IdIFZS = 0x49465a53 
  val IdIFhd = 0x49466864
  val IdCMem = 0x434d656d
  val IdUMem = 0x554d656d
  val IdStks = 0x53746b73
}

class QuetzalWriter(vmState: VMStateImpl) {
  import QuetzalCompression._

  val byteOut = new ByteArrayOutputStream
  val out = new DataOutputStream(byteOut)

  def write(outputStream: java.io.OutputStream): Boolean = {
    if (outputStream != null) {
      writeIffHeader
      var numFormBytes = 8
      numFormBytes += writeIFhdChunk
      numFormBytes += writeCMemChunk
      numFormBytes += writeStksChunk
      out.flush
      out.close
      val dataBytes: Array[Byte] = byteOut.toByteArray
      val formSize = dataBytes.length - 8
      dataBytes(4) = ((formSize >>> 24) & 0xff).asInstanceOf[Byte]
      dataBytes(5) = ((formSize >>> 16) & 0xff).asInstanceOf[Byte]
      dataBytes(6) = ((formSize >>> 8) & 0xff).asInstanceOf[Byte]
      dataBytes(7) = (formSize & 0xff).asInstanceOf[Byte]
      printf("# form bytes = %d\n", formSize)
      try {
        outputStream.write(dataBytes)
        outputStream.flush
      } finally {
        outputStream.close
      }
    }
    outputStream != null
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
    out.writeByte((vmState.pc >>> 16) & 0xff)
    out.writeChar(vmState.pc & 0xffff)
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
    out.writeInt(numStackFrameBytes)
    for (i <- stackFrames.size - 1 to 0 by - 1) {
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

      printf("CURRENT FP IS: %d, RET PC = $%04x OLDFP = %d, STOREVAR = %d, # args = %d, # locals: %d\n",
             currentFp, vmState.stack.value32At(currentFp + ReturnPC),
             vmState.stack.valueAt(currentFp + OldFP),
             vmState.stack.valueAt(currentFp + StoreVar),
             vmState.stack.valueAt(currentFp + NumArgs),
             numLocals)
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

class QuetzalReader(vmState: VMStateImpl, machine: Machine) {
  import QuetzalCompression._
  import QuetzalFormat._

  def read(in: java.io.InputStream): Boolean = {
    if (in != null) {
      var dataIn: DataInputStream = null
      try {
        dataIn = new DataInputStream(in)
        val formInt = dataIn.readInt
        if (formInt == IdFORM) {
          val numBytes = dataIn.readInt
          println("valid file, # bytes to read: " + numBytes)
          val iffType = dataIn.readInt
          if (iffType == IdIFZS) {
            var numBytesRead = 4
            while (numBytesRead < numBytes) numBytesRead += readChunk(dataIn)
          } else throw new IllegalFormatException
        } else throw new IllegalFormatException
      } catch {
        case e:NonMatchingGameSaveException =>
          machine.warn("Save file was not created for this game\n")
          return false
        case e:IllegalFormatException =>
          machine.warn("Save file is not in Quetzal format\n")
          return false
        case _ =>
          machine.warn("Could not read save file\n")
          return false
      } finally {
        if (dataIn != null) dataIn.close
      }
    }
    true
  }

  private def readChunk(dataIn: DataInputStream): Int = {
    val chunkType = dataIn.readInt
    chunkType match {
      case IdIFhd =>
        println("IFhd recognized")
        readIFhdChunk(dataIn)
      case IdCMem =>
        println("CMem recognized")
        readCMemChunk(dataIn)
      case IdStks =>
        println("Stks recognized")
        readStksChunk(dataIn)
      case _ =>
        println("unknown tag")
        464 // debug
    }
  }

  private def readIFhdChunk(dataIn: DataInputStream): Int = {
    val chunkLength = dataIn.readInt
    val release = dataIn.readChar.asInstanceOf[Int]
    val serial = new Array[Int](6)
    for (i <- 0 until 6) serial(i) = dataIn.readByte & 0xff
    val checksum = dataIn.readChar.asInstanceOf[Int]
    val pcHi = dataIn.readByte & 0xff
    val pcLo = dataIn.readChar.asInstanceOf[Int]
    val restorePc = (pcHi << 16) | pcLo
    dataIn.readByte // skip pad byte
    printf("IFhd read, restore PC = $%04x\n", restorePc)
    verifyStory(release, serial, checksum)
    vmState.pc = restorePc
    22
  }

  private def verifyStory(release: Int, serial: Array[Int], checksum: Int) {
    if (release != vmState.header.releaseNumber ||
        !checkSerial(serial) || checksum != vmState.header.checksum) {
          throw new NonMatchingGameSaveException
        }
  }
  private def checkSerial(serial: Array[Int]): Boolean = {
    val originalSerial = vmState.header.serialNumber
    for (i <- 0 until 6) if (serial(i) != originalSerial(i)) return false
    true
  }

  private def readCMemChunk(dataIn: DataInputStream): Int = {
    val chunkLength = dataIn.readInt
    val compressed = new Array[Byte](chunkLength)

    var pos = 0
    var bytesToRead = chunkLength
    while (bytesToRead > 0) {
      val bytesRead = dataIn.read(compressed, pos, bytesToRead)
      pos += bytesRead
      bytesToRead -= bytesRead
    }
    decompressDiffBytes(compressed,
                        vmState.originalDynamicMem,
                        vmState.storyData,
                        vmState.header.staticStart)
    
    if ((chunkLength % 2) == 0) chunkLength + 8
    else chunkLength + 8 + 1
  }

  private def readUMemChunk(dataIn: DataInputStream): Int = {
    0
  }
  private def readStksChunk(dataIn: DataInputStream): Int = {
    import FrameOffset._

    val chunkLength = dataIn.readInt
    var bytesToRead = chunkLength
    var numEvalWords = 0
    var sp = 0
    var oldFP = -1
    if (vmState.header.version != 6) { // read in dummy frame
      dataIn.skipBytes(6)
      numEvalWords = dataIn.readChar
      for (i <- 0 until numEvalWords) {
        vmState.stack.setValueAt(sp, dataIn.readChar)
        sp += 1
      }
      bytesToRead -= (8 + numEvalWords * 2)
    }
    while (bytesToRead > 0) {
      // read stack frame
      val pc = (dataIn.readByte << 16) | dataIn.readChar
      val flags = dataIn.readByte
      var storeVariable = dataIn.readByte
      if ((flags & 0x10) == 0x10) storeVariable = -1
      val numLocals = flags & 0x0f
      val numArgs = getNumArgs(dataIn.readByte)
      numEvalWords = dataIn.readChar

      // advance frame pointer, so at the end, it will point at
      // the correct one
      vmState.fp = sp
      vmState.stack.setValueAt(sp, pc)
      vmState.stack.setValueAt(sp + OldFP, oldFP)
      vmState.stack.setValueAt(sp + StoreVar, storeVariable)
      vmState.stack.setValueAt(sp + NumArgs, numArgs)
      vmState.stack.setValueAt(sp + NumLocals, numLocals)
      sp += 5
      for (i <- 0 until numLocals) {
        vmState.stack.setValueAt(sp, dataIn.readChar)
        sp += 1
      }
      for (i <- 0 until numEvalWords) {
        vmState.stack.setValueAt(sp, dataIn.readChar)
        sp += 1
      }
      oldFP = vmState.fp
      bytesToRead -= (8 + numLocals * 2 + numEvalWords * 2)
    }
    vmState.stack.sp = sp
    chunkLength + 8
  }

  private def getNumArgs(argDescriptor: Int) = {
    var result = 0
    for (i <- 0 until 7) {
      val mask = 1 << i
      if ((argDescriptor & mask) == mask) result += 1
    }
    result
  }
}
