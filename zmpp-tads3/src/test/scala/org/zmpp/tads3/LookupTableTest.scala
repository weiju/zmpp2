/*
 * Created on 2010/12/13
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
class LookupTableSpec extends FlatSpec with BeforeAndAfterEach {
  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null

  override def beforeEach {
    objectSystem = new ObjectSystem
    functionSetMapper = new IntrinsicFunctionSetMapper
    vmState = new TadsVMState(objectSystem, functionSetMapper)
  }
  "LookupTable" should "be created" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    assert(lookupTable.entryCount === 0)
  }

  it should "add a value" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    lookupTable(T3Integer(3)) = T3Integer(42)

    assert(lookupTable.entryCount === 1)
    assert(lookupTable.isKeyPresent(T3Integer(3)))
    assert(lookupTable(T3Integer(3)) == T3Integer(42))
    assert(lookupTable.valueAtIndex(T3Integer(3)) == T3Integer(42))
  }
  it should "add a value through setValueAtIndex" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    val id = lookupTable.setValueAtIndex(T3Integer(3), T3Integer(42))

    assert(lookupTable.entryCount === 1)
    assert(lookupTable.isKeyPresent(T3Integer(3)))
    assert(lookupTable.valueAtIndex(T3Integer(3)) == T3Integer(42))
    assert(lookupTable(T3Integer(3)) == T3Integer(42))
    assert(id == T3ObjectId(1))
  }
  it should "update a value" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    lookupTable(T3Integer(3)) = T3Integer(42)
    lookupTable(T3Integer(3)) = T3Integer(43)

    assert(lookupTable.entryCount === 1)
    assert(lookupTable.isKeyPresent(T3Integer(3)))
    assert(lookupTable(T3Integer(3)) == T3Integer(43))
  }
  it should "add two values" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    lookupTable(T3Integer(3)) = T3Integer(42)
    lookupTable(T3Integer(4)) = T3Integer(43)

    assert(lookupTable.entryCount === 2)
    assert(lookupTable.isKeyPresent(T3Integer(3)))
    assert(lookupTable.isKeyPresent(T3Integer(4)))
    assert(lookupTable(T3Integer(3)) == T3Integer(42))
    assert(lookupTable(T3Integer(4)) == T3Integer(43))
  }
  it should "remove a value" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    lookupTable(T3Integer(3)) = T3Integer(42)

    val result = lookupTable.removeElement(T3Integer(3))
    assert(result == T3Integer(42))
    assert(lookupTable.entryCount === 0)
    assert(!lookupTable.isKeyPresent(T3Integer(3)))
  }
  it should "remove a non-existing value" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    lookupTable(T3Integer(3)) = T3Integer(42)
      
    val result = lookupTable.removeElement(T3Integer(5))
    assert(result === T3Nil)
    assert(lookupTable.entryCount === 1)
    assert(lookupTable.isKeyPresent(T3Integer(3)))
  }
  it should "invoke keysToList" in {
    val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
    lookupTable(T3Integer(3)) = T3Integer(42)
    lookupTable(T3Integer(5)) = T3Integer(43)

    val listId = lookupTable.keysToList
    val keys = objectSystem.objectWithId(listId).asInstanceOf[TadsList]
    assert(keys.size === 2)
  }
}
