/*
 * Created on 2010/11/07
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

class ObjectModelTest extends JUnit4(ObjectModelSpec)
object ObjectModelSpecRunner extends ConsoleRunner(ObjectModelSpec)

object ObjectModelSpec extends Specification {
  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null

  "T3ObjectId" should {
    "be equal" in {
      val objId42      = new T3ObjectId(42)
      val objId43      = new T3ObjectId(43)
      val objId42too   = new T3ObjectId(42)
      val objId42three = T3Value.create(TypeIds.VmObj, 42)
      val int42        = T3Value.create(TypeIds.VmInt, 42)

      objId42      must_== objId42
      objId42      must_== objId42too
      objId42too   must_== objId42
      objId42      must_== objId42three
      objId42three must_== objId42

      objId42      must_!= objId43
      objId43      must_!= objId42
      int42        must_!= objId42
      objId42      must_!= int42
    }
  }
  "ObjectSystem" should {
    "be initialized" in {
      val objectSystem = new ObjectSystem
      val id1 = objectSystem.newObjectId
      id1.value     must_== 1
      id1.valueType must_== TypeIds.VmObj
    }
  }

  "TadsObject" should {
    doBefore {
      objectSystem      = new ObjectSystem
      functionSetMapper = new IntrinsicFunctionSetMapper
      vmState = new TadsVMState(objectSystem, functionSetMapper)
    }
    "be created" in {
      val obj = new TadsObject(new T3ObjectId(1), vmState, false, 0, 0, false)
      obj.metaClass.name must_== "tads-object"
    }
    "get non-existing" in {
      val obj = new TadsObject(new T3ObjectId(1), vmState, false, 0, 0, false)
      obj.getProperty(2831, 0) must_== InvalidProperty
      obj.numProperties must_== 0
    }
    "set non-existing" in {
      val obj = new TadsObject(new T3ObjectId(1), vmState, false, 0, 0, false)
      val testVal = new T3Integer(4711)
      obj.setProperty(2831, testVal)
      obj.numProperties must_== 1
      obj.getProperty(2831, 0).tadsValue must_== testVal
    }
    "overwrite existing" in {
      val obj = new TadsObject(new T3ObjectId(1), vmState, false, 0, 0, false)
      val testVal1 = new T3Integer(4711)
      val testVal2 = new T3Integer(4712)
      obj.setProperty(2831, testVal1)
      obj.setProperty(2831, testVal2)
      obj.numProperties must_== 1
      obj.getProperty(2831, 0).tadsValue must_== testVal2
    }
  }
}
