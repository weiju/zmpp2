/*
 * Created on 2010/11/07
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
class TadsObjectIdSpec extends FlatSpec {

  "T3ObjectId" should "be equal" in {
    val objId42      = T3ObjectId(42)
    val objId43      = T3ObjectId(43)
    val objId42too   = T3ObjectId(42)
    val objId42three = T3Value.create(TypeIds.VmObj, 42)
    val int42        = T3Value.create(TypeIds.VmInt, 42)

    assert(objId42 == objId42)
    assert(objId42 == objId42too)
    assert(objId42too == objId42)
    assert(objId42 == objId42three)
    assert(objId42three == objId42)
    
    assert(objId42  != objId43)
    assert(objId43  != objId42)
    assert(int42 != objId42)
    assert(objId42 != int42)
  }
}

@RunWith(classOf[JUnitRunner])
class ObjectSystemSpec extends FlatSpec {

  "ObjectSystem" should "be initialized" in {
    val objectSystem = new ObjectSystem
    val id1 = objectSystem.newObjectId
    assert(id1.value === 1)
    assert(id1.valueType == TypeIds.VmObj)
  }
}

@RunWith(classOf[JUnitRunner])
class TadsObjectSpec extends FlatSpec with BeforeAndAfterEach {

  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null

  override def beforeEach {
    objectSystem      = new ObjectSystem
    functionSetMapper = new IntrinsicFunctionSetMapper
    vmState = new TadsVMState(objectSystem, functionSetMapper)
  }

  "TadsObject" should "be created" in {
    val obj = new TadsObject(T3ObjectId(1), vmState, false, 0, 0, false)
    assert(obj.metaClass.name == "tads-object")
  }
  it should "get non-existing" in {
    val obj = new TadsObject(T3ObjectId(1), vmState, false, 0, 0, false)
    assert(obj.getProperty(2831, 0) === InvalidProperty)
    assert(obj.numProperties === 0)
  }
  it should "set non-existing" in {
    val obj = new TadsObject(T3ObjectId(1), vmState, false, 0, 0, false)
    val testVal = T3Integer(4711)
    obj.setProperty(2831, testVal)

    assert(obj.numProperties === 1)
    assert(obj.getProperty(2831, 0).tadsValue == testVal)
  }
  it should "overwrite existing" in {
    val obj = new TadsObject(T3ObjectId(1), vmState, false, 0, 0, false)
    val testVal1 = T3Integer(4711)
    val testVal2 = T3Integer(4712)
    obj.setProperty(2831, testVal1)
    obj.setProperty(2831, testVal2)

    assert(obj.numProperties === 1)
    assert(obj.getProperty(2831, 0).tadsValue == testVal2)
  }
}
