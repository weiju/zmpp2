/*
 * Created on 2010/11/28
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
package org.zmpp.tads3

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class TadsStringTest extends JUnit4(TadsStringSpec)
object TadsStringSpecRunner extends ConsoleRunner(TadsStringSpec)

object TadsStringSpec extends Specification {
  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null
  
  def makeString(id: Int, str: String) = {
    val result = new TadsString(new T3ObjectId(id), vmState, false)
    result.init(str)
    result
  }

  "TadsString" should {
    doBefore {
      objectSystem = new ObjectSystem
      functionSetMapper = new IntrinsicFunctionSetMapper
      vmState = new TadsVMState(objectSystem, functionSetMapper)
    }
    "be created" in {
      makeString(1, "astring").length must_== "astring".length
    }
    "be concatenated" in {
      val str1 = makeString(1, "Hello, ")
      val str2 = makeString(2, "World !")
      (str1 + str2).asInstanceOf[TadsString].string must_== "Hello, World !"
    }
    "do a find" in {
      val str1 = makeString(1, "Hello, World !")
      val str2 = makeString(3, "ello")
      val str3 = makeString(2, "salami")
      // success
      str1.find(str1, 1) must_== 1
      str1.find(str2, 1) must_== 2
      // failure
      str1.find(str2, 3) must_== 0
      str1.find(str3, 1) must_== 0
    }
    "do a findReplace" in {
      val str1 = makeString(1, "blablabla")
      str1.findReplace(makeString(2, "bla"),
                       makeString(3, "rhabarber"), true, 1).string must_==
        "rhabarberrhabarberrhabarber"
      str1.findReplace(makeString(2, "bla"),
                       makeString(3, "rhabarber"), false, 2).string must_==
        "blarhabarberbla"
      str1.findReplace(makeString(2, "bla"),
                       makeString(3, "rhabarber"), true, 2).string must_==
        "blarhabarberrhabarber"
    }
    "do a substr" in {
      val str1 = makeString(1, "abcdef")
      str1.substr(3).string must_== "cdef"
      str1.substr(3, 2).string must_== "cd"
      val str2 = makeString(2, "abcdefghi")
      str2.substr(-3).string must_== "ghi"
      str2.substr(-3, 2).string must_== "gh"
      str2.substr(-3, 5).string must_== "ghi"
      str2.substr(1, 0).string must_== ""
    }
  }
}
