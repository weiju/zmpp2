/*
 * Created on 2010/04/22
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
package org.zmpp.glk

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class GlkFileTest extends JUnit4(GlkFileSpec)
object GlkFileSpecRunner extends ConsoleRunner(GlkFileSpec)

object GlkFileSpec extends Specification {

  "GlkFileStream" should {
    var fileStream: GlkFileStream = null

    doBefore {
      val tmpfile = File.createTempFile("zmppfiletest", "tmp")
      val fileRef = new FileReference(1, 0, 1, tmpfile, 0)
      fileStream = new GlkFileStream(fileRef, 1, 0, false)
    }
    doAfter {
      fileStream.close
    }

    "be initialized" in {
      fileStream.id         must_== 0
      fileStream.writeCount must_== 0
      fileStream.readCount  must_== 0
      fileStream.size       must_== 0
    }
    "put a character" in {
      fileStream.putChar('a')
      fileStream.readCount  must_== 0
      fileStream.writeCount must_== 1
      fileStream.size       must_== 1
    }
    "resize buffer" in {
      val numToWrite = 2000
      for (i <- 0 until numToWrite) fileStream.putChar('a')
      fileStream.size       must_== numToWrite
      fileStream.writeCount must_== numToWrite
      fileStream.position   must_== numToWrite
    }
    "seek from start" in {
      for (i <- 0 until 5) fileStream.putChar('a')
      fileStream.seek(3, SeekModes.Start)
      fileStream.size       must_== 5
      fileStream.writeCount must_== 5
      fileStream.position   must_== 3
    }
    "seek from current" in {
      for (i <- 0 until 5) fileStream.putChar('a')
      fileStream.seek(3, SeekModes.Current)
      fileStream.size       must_== 5
      fileStream.writeCount must_== 5
      fileStream.position   must_== 8
    }
    "seek from end" in {
      for (i <- 0 until 5) fileStream.putChar('a')
      fileStream.seek(0, SeekModes.Start)
      fileStream.seek(3, SeekModes.End)
      fileStream.size       must_== 5
      fileStream.writeCount must_== 5
      fileStream.position   must_== 8
    }
  }
}

