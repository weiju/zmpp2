/*
 * Created on 2010/11/25
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
class TadsListSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null

  def toTadsList(value: T3Value) = objectSystem.toTadsList(value)

  override def beforeEach {
    objectSystem = new ObjectSystem
    functionSetMapper = new IntrinsicFunctionSetMapper
    vmState = new TadsVMState(objectSystem, functionSetMapper)
  }

  "TadsList" should "be created" in {
    val list = new TadsList(T3ObjectId(1), vmState, false)
    list.size should equal (0)
  }
  it should "be initialized" in {
    val list = new TadsList(T3ObjectId(1), vmState, false)
    val one = T3Integer(1)
    val two = T3Integer(2)
    val three = T3Integer(3)
    list.initWith(List(one, two, three))

    list.size                       should equal (3)
    list.valueAtIndex(T3Integer(1)) should equal (T3Integer(1))
    list.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    list.valueAtIndex(T3Integer(3)) should equal (T3Integer(3))
  }
  it should "be sorted ascending using standard comparison" in {
    val list = new TadsList(T3ObjectId(1), vmState, false)
    val one = T3Integer(1)
    val two = T3Integer(2)
    val three = T3Integer(3)
    list.initWith(List(two, three, one))
    val newList = objectSystem.toTadsList(list.sort(desc = false, compFunc = T3Nil))

    newList.size                       should equal (3)
    newList.valueAtIndex(T3Integer(1)) should equal (T3Integer(1))
    newList.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    newList.valueAtIndex(T3Integer(3)) should equal (T3Integer(3))
  }
  it should "be sorted descending using standard comparison" in {
    val list = new TadsList(T3ObjectId(1), vmState, false)
    val one = T3Integer(1)
    val two = T3Integer(2)
    val three = T3Integer(3)
    list.initWith(List(two, three, one))
    val newList = toTadsList(list.sort(desc = true, compFunc = T3Nil))

    newList.size                       should equal (3)
    newList.valueAtIndex(T3Integer(1)) should equal (T3Integer(3))
    newList.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    newList.valueAtIndex(T3Integer(3)) should equal (T3Integer(1))
  }
  it should "use the + operation with a simple value" in {
    val list = new TadsList(T3ObjectId(1), vmState, false)
    val one = T3Integer(1)
    val two = T3Integer(2)
    val three = T3Integer(3)
    list.initWith(List(one, two))
    val newList = toTadsList(list + three)

    newList.size                       should equal (3)
    newList                            should not equal (list)
    newList.valueAtIndex(T3Integer(1)) should equal (T3Integer(1))
    newList.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    newList.valueAtIndex(T3Integer(3)) should equal (T3Integer(3))
  }
  it should "use the + operation with a non-list object value" in {
    val list = new TadsList(T3ObjectId(1), vmState, false)
    val one = T3Integer(1)
    val two = T3Integer(2)
    val str = new TadsString(T3ObjectId(3), vmState, false)
    objectSystem.registerObject(str)
    list.initWith(List(one, two))
    val newList = toTadsList(list + str.id)

    newList.size                       should equal (3)
    newList                            should not equal (list)
    newList.valueAtIndex(T3Integer(1)) should equal (T3Integer(1))
    newList.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    newList.valueAtIndex(T3Integer(3)) should equal (T3ObjectId(3))
  }
  it should "use the + operation with a list value" in {
    val list1 = new TadsList(T3ObjectId(1), vmState, false)
    val one   = T3Integer(1)
    val two   = T3Integer(2)
    val three = T3Integer(3)
    val four  = T3Integer(4)
    val list2 = new TadsList(T3ObjectId(2), vmState, false)
    objectSystem.registerObject(list2)
    list1.initWith(List(one, two))
    list2.initWith(List(three, four))
    val newList = toTadsList(list1 + list2.id)

    newList.size                       should equal (4)
    newList                            should not equal (list1)
    newList.valueAtIndex(T3Integer(1)) should equal (T3Integer(1))
    newList.valueAtIndex(T3Integer(2)) should equal (T3Integer(2))
    newList.valueAtIndex(T3Integer(3)) should equal (T3Integer(3))
    newList.valueAtIndex(T3Integer(4)) should equal (T3Integer(4))
  }
}
