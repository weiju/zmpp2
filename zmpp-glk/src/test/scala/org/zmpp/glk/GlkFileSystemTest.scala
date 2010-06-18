/*
 * Created on 2010/04/22
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
package org.zmpp.glk

import org.specs._
import org.specs.matcher._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class GlkFileSystemTest extends JUnit4(GlkFileSystemSpec)
object GlkFileSystemSpecRunner extends ConsoleRunner(GlkFileSystemSpec)

/**
 * Note: We compare with xUnit matchers, there seems to be a Scala/Specs bug, which
 * tries to use String.isEmpty which only exists in Java SE 6
 */
object GlkFileSystemSpec extends Specification with xUnit {
  "GlkFileSystem" should {
    "be initialized" in {
      val fileSystem = new GlkFileSystem
      assertNull(fileSystem.iterate(0))
    }
    "create a fileref by name" in {
      val fileSystem = new GlkFileSystem
      fileSystem.createFileRefByName(0, "myfile", 0)
      val fileRef = fileSystem.iterate(0)
      assertNotNull(fileRef)
      assertNull(fileSystem.iterate(fileRef.id))
    }
    /*
    "register a stream and close it" in {
      val ioSystem = new GlkIOSystem
      val stream = new DummyStream
      ioSystem.registerStream(stream)
      ioSystem.closeStream(stream.id)
      assertTrue(ioSystem.iterate(0) == null)
    }
    "register two streams" in {
      val ioSystem = new GlkIOSystem
      val stream1 = new DummyStream
      val stream2 = new DummyStream
      ioSystem.registerStream(stream1)
      ioSystem.registerStream(stream2)
      assertTrue(ioSystem.iterate(0) == stream2)
      assertTrue(ioSystem.iterate(stream2.id) == stream1)
      assertNull(ioSystem.iterate(stream1.id))
    }*/
  }
}
