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
package org.zmpp.iff

import java.io.{ByteArrayOutputStream, DataOutputStream}

/**
 * Generic implementation of the algorithm for compressing memory.
 */
object QuetzalCompression {
  def decompressDiffBytes(compressed: Array[Byte],
                          originalBytes: Array[Byte],
                          targetBytes: Array[Byte],
                          numBytes: Int) = {
    System.arraycopy(originalBytes, 0, targetBytes, 0, numBytes)
    var compressedIndex = 0
    var targetIndex = 0
    while (compressedIndex < compressed.length) {
      if (compressed(compressedIndex) == 0) {
        val numZeroBytes = (compressed(compressedIndex + 1) & 0xff) + 1
        targetIndex += numZeroBytes
        compressedIndex += 1
      } else {
        targetBytes(targetIndex) =
          (originalBytes(targetIndex) ^ compressed(compressedIndex)).asInstanceOf[Byte]
        targetIndex += 1
      }
      compressedIndex += 1
    }
    targetBytes
  }

  def compressDiffBytes(changedBytes: Array[Byte],
                        originalBytes: Array[Byte],
                        numBytes: Int) = {
    val resultStream = new ByteArrayOutputStream
    val out = new DataOutputStream(resultStream)
    var srcIndex = 0
    var zeroCount = 0
    printf("Compress Data: \n[")
    def writeZeroRun {
      out.writeByte(0)
      printf("(0, %d), ", zeroCount - 1)
      out.writeByte(((zeroCount - 1) & 0xff).asInstanceOf[Byte])
      zeroCount = 0
    }
    while (srcIndex < numBytes) {
      val xorValue = (changedBytes(srcIndex) ^ originalBytes(srcIndex)) & 0xff
      if (xorValue == 0) {
        zeroCount += 1
        // number of zeros exceeds range of a byte, write a run
        if (zeroCount == 256) writeZeroRun
      } else {
        if (zeroCount > 0) writeZeroRun
        printf("[%d], ", xorValue)
        out.writeByte(xorValue)
      }
      srcIndex += 1
    }
    printf("]\n\n")
    out.flush
    out.close
    resultStream.toByteArray
  }
}
