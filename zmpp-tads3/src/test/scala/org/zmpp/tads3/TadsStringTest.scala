/*
 * Created on 2010/11/28
 * Copyright (c) 2010-2014, Wei-ju Wu.
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

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

@RunWith(classOf[JUnitRunner])
class TadsStringSpec extends FlatSpec with BeforeAndAfterEach {

  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null
  
  override def beforeEach {
    objectSystem = new ObjectSystem
    functionSetMapper = new IntrinsicFunctionSetMapper
    vmState = new TadsVMState(objectSystem, functionSetMapper)
  }
  "TadsString" should "be created" in {
    assert(makeString(1, "astring").length === "astring".length)
  }
  it should "be concatenated" in {
    val str1 = makeString(1, "Hello, ")
    val str2 = makeString(2, "World !")
    val str3Id = str1 + str2.id
    assert(objectSystem.toTadsString(str3Id).string == "Hello, World !")
  }
  it should "do a find" in {
    val str1 = makeString(1, "Hello, World !")
    val str2 = makeString(3, "ello")
    val str3 = makeString(2, "salami")

    // success
    assert(str1.find(str1, 1) === 1)
    assert(str1.find(str2, 1) === 2)
    // failure
    assert(str1.find(str2, 3) === 0)
    assert(str1.find(str3, 1) === 0)
  }
  it should "do a findReplace" in {
    val str1 = makeString(1, "blablabla")
    assert( str1.findReplace(makeString(2, "bla"),
                     makeString(3, "rhabarber"), true, 1).string ==
     "rhabarberrhabarberrhabarber")
    assert( str1.findReplace(makeString(2, "bla"),
                       makeString(3, "rhabarber"), false, 2).string ==
     "blarhabarberbla")
    assert( str1.findReplace(makeString(2, "bla"),
                       makeString(3, "rhabarber"), true, 2).string ==
     "blarhabarberrhabarber")
  }
  it should "do a substr" in {
    val str1 = makeString(1, "abcdef")
    assert(str1.substr(3).string == "cdef")
    assert(str1.substr(3, 2).string == "cd")

    val str2 = makeString(2, "abcdefghi")
    assert(str2.substr(-3).string == "ghi")
    assert(str2.substr(-3, 2).string == "gh")
    assert(str2.substr(-3, 5).string == "ghi")
    assert(str2.substr(1, 0).string  == "")
  }
  it should "do substr with start = 0 (undocumented)" in {
    assert(makeString(1, "abcdef").substr(0, 1).string == "a")
  }
  it should "when containing invisible char not equal to empty string" in {
    // this is a strange case that happened while
    // developing, we keep it to catch it in the future
    val str1 = makeString(1, "\u000f")
    val str2 = makeString(2, "")
    assert(str1 != str2)
  }
  it should "perform endsWith()" in {
    val str1 = makeString(1, "HelloWorld")
    val str2 = makeString(2, "World")
    val str3 = makeString(3, "Welt")
      
    assert(str1.endsWith(str2))
    assert(!str1.endsWith(str3))
  }
  it should "perform startsWith()" in {
    val str1 = makeString(1, "HelloWorld")
    val str2 = makeString(2, "Hello")
    val str3 = makeString(3, "Hallo")
      
    assert(str1.startsWith(str2))
    assert(!str1.startsWith(str3))
  }

  private def makeString(id: Int, str: String) = {
    val result = new TadsString(T3ObjectId(id), vmState, false)
    result.init(str)
    objectSystem.registerObject(result)
    result
  }
}
