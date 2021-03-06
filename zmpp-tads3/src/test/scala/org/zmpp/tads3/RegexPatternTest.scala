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
class RegexPatternSpec extends FlatSpec with BeforeAndAfterEach {

  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var objectSystem : ObjectSystem = null
  var vmState : TadsVMState = null

  override def beforeEach {
    objectSystem = new ObjectSystem
    functionSetMapper = new IntrinsicFunctionSetMapper
    vmState = new TadsVMState(objectSystem, functionSetMapper)
  }

  "RegexPattern" should "be created" in {
    val pattern = makePattern(1, "apattern")
    assert(pattern.javaPatternString === "apattern")
    assert(!pattern.ignoreCase)
  }
  it should "use the ignoreCase flag" in {
    val pattern = makePattern(1, "<nocase>apattern")
    assert(pattern.javaPatternString === "apattern")
    assert(pattern.ignoreCase)
  }
  it should "translate a pattern" in {
    val pattern = makePattern(10, "<nocase><langle>%.(/?[a-z][a-z0-9]*)<rangle>")
    pattern.compile

    assert(pattern.javaPatternString === "[<]\\.(/?[a-z][a-z0-9]*)[>]")

    val s1 = pattern.search(makeString(1, "<.p0><title>"), 1)
    assert(s1.valueAtIndex(T3Integer(1)) ==  T3Integer(1))
    assert(s1.valueAtIndex(T3Integer(2)) == T3Integer(5))

    val s2 = pattern.search(makeString(2, "some<.p0><title>"), 1)
    assert(s2.valueAtIndex(T3Integer(1)) == T3Integer(5))
    assert(s2.valueAtIndex(T3Integer(2)) == T3Integer(5))
    assert(pattern.search(makeString(2, "sometext"), 1) === null)
  }
  it should "return a group" in {
    val pattern = makePattern(10, "<nocase><langle>%.(/?[a-z][a-z0-9]*)<rangle>")
    pattern.search(makeString(1, "<.p0><title>"), 1)
    val matchGroup = pattern.group(1)

    assert(matchGroup.size === 3)
    assert(matchGroup.valueAtIndex(T3Integer(1)) == T3Integer(3))
    assert(matchGroup.valueAtIndex(T3Integer(2)) == T3Integer(2))
    assert(matchGroup.valueAtIndex(T3Integer(3)).valueType == TypeIds.VmObj)
  }

  // returns a pattern which is retrieving the specified string from a
  // mocked ObjectSystem
  def makePattern(id: Int, str: String) = {
    val mockObjectSystem = new ObjectSystem {
      override def stringConstantWithOffset(offset: T3SString): TadsString = {
        val t3str = new TadsString(T3ObjectId(1), vmState, false)
        t3str.init(str)
        t3str
      }
    }
    val mockVmState = new TadsVMState(mockObjectSystem, functionSetMapper)
    val result = new RegexPattern(T3ObjectId(id), mockVmState, false)
    result.init(T3SString(4711)) // only a dummy
    result
  }

  def makeString(id: Int, str: String) = {
    val result = new TadsString(T3ObjectId(id), vmState, false)
    result.init(str)
    result
  }
}
