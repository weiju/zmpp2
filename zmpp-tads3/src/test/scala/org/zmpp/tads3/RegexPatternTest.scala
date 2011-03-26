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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

@RunWith(classOf[JUnitRunner])
class RegexPatternSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

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
    pattern.javaPatternString should equal ("apattern")
    pattern.ignoreCase        should be (false)
  }
  it should "use the ignoreCase flag" in {
    val pattern = makePattern(1, "<nocase>apattern")
    pattern.javaPatternString should equal ("apattern")
    pattern.ignoreCase        should be (true)
  }
  it should "translate a pattern" in {
    val pattern = makePattern(10, "<nocase><langle>%.(/?[a-z][a-z0-9]*)<rangle>")
    pattern.compile

    pattern.javaPatternString should equal ("[<]\\.(/?[a-z][a-z0-9]*)[>]")

    val s1 = pattern.search(makeString(1, "<.p0><title>"), 1)
    s1.valueAtIndex(T3Integer(1)) should equal (T3Integer(1))
    s1.valueAtIndex(T3Integer(2)) should equal (T3Integer(5))

    val s2 = pattern.search(makeString(2, "some<.p0><title>"), 1)
    s2.valueAtIndex(T3Integer(1)) should equal (T3Integer(5))
    s2.valueAtIndex(T3Integer(2)) should equal (T3Integer(5))
    pattern.search(makeString(2, "sometext"), 1) should be (null)
  }
  it should "return a group" in {
    val pattern = makePattern(10, "<nocase><langle>%.(/?[a-z][a-z0-9]*)<rangle>")
    pattern.search(makeString(1, "<.p0><title>"), 1)
    val matchGroup = pattern.group(1)

    matchGroup.size should equal (3)
    matchGroup.valueAtIndex(T3Integer(1)) should equal (T3Integer(3))
    matchGroup.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    matchGroup.valueAtIndex(T3Integer(3)).valueType should equal (TypeIds.VmObj)
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
