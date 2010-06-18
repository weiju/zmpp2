/*
 * Created on 2010/04/01
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

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

import java.io._

class StackTest extends JUnit4(StackSpec)
object StackSpecRunner extends ConsoleRunner(StackSpec)

object StackSpec extends Specification {

  "Stack" should {
    "be initialized" in {
      val stack = new Stack(10)
      stack.empty must beTrue
    }
    "push and pop byte" in {
      val stack = new Stack(10)
      stack.pushByte(1)
      stack.empty must beFalse
      stack.topByte must_== 1
      stack.empty must beFalse
      stack.popByte must_== 1
      stack.empty must beTrue
      stack.pushByte(255)
      stack.topByte must_== 255
      stack.popByte must_== 255
    }
    "push and pop short" in {
      val stack = new Stack(10)
      stack.pushShort(32767)
      stack.topShort must_== 32767
      stack.pushShort(65535)
      stack.topShort must_== 65535
      stack.popShort must_== 65535
      stack.popShort must_== 32767
      stack.empty must beTrue
    }
    "push and pop int" in {
      val stack = new Stack(10)
      stack.pushInt(32767)
      stack.topInt must_== 32767
      stack.pushInt(-42)
      stack.topInt must_== -42
      stack.popInt must_== -42
      stack.popInt must_== 32767
      stack.empty must beTrue
    }
  }
}

