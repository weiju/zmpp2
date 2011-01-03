/*
 * Created on 2010/12/13
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

class LookupTableTest extends JUnit4(LookupTableSpec)
object LookupTableSpecRunner extends ConsoleRunner(LookupTableSpec)

object LookupTableSpec extends Specification {
  var objectSystem : ObjectSystem = null
  var functionSetMapper : IntrinsicFunctionSetMapper = null
  var vmState : TadsVMState = null

  "LookupTable" should {
    doBefore {
      objectSystem = new ObjectSystem
      functionSetMapper = new IntrinsicFunctionSetMapper
      vmState = new TadsVMState(objectSystem, functionSetMapper)
    }
    "be created" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      lookupTable.entryCount must_== 0
    }
    "add a value" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      lookupTable(T3Integer(3)) = T3Integer(42)

      lookupTable.entryCount must_== 1
      lookupTable.isKeyPresent(T3Integer(3)) must beTrue
      lookupTable(T3Integer(3)) must_== T3Integer(42)
      lookupTable.valueAtIndex(T3Integer(3)) must_== T3Integer(42)
    }
    "add a value through setValueAtIndex" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      val id = lookupTable.setValueAtIndex(T3Integer(3), T3Integer(42))

      lookupTable.entryCount must_== 1
      lookupTable.isKeyPresent(T3Integer(3)) must beTrue
      lookupTable.valueAtIndex(T3Integer(3)) must_== T3Integer(42)
      lookupTable(T3Integer(3)) must_== T3Integer(42)
      id must_== T3ObjectId(1)
    }
    "update a value" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      lookupTable(T3Integer(3)) = T3Integer(42)
      lookupTable(T3Integer(3)) = T3Integer(43)

      lookupTable.entryCount must_== 1
      lookupTable.isKeyPresent(T3Integer(3)) must beTrue
      lookupTable(T3Integer(3)) must_== T3Integer(43)
    }
    "add two values" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      lookupTable(T3Integer(3)) = T3Integer(42)
      lookupTable(T3Integer(4)) = T3Integer(43)

      lookupTable.entryCount must_== 2
      lookupTable.isKeyPresent(T3Integer(3)) must beTrue
      lookupTable.isKeyPresent(T3Integer(4)) must beTrue
      lookupTable(T3Integer(3)) must_== T3Integer(42)
      lookupTable(T3Integer(4)) must_== T3Integer(43)
    }
    "remove a value" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      lookupTable(T3Integer(3)) = T3Integer(42)

      val result = lookupTable.removeElement(T3Integer(3))
      result must_== T3Integer(42)
      lookupTable.entryCount must_== 0
      lookupTable.isKeyPresent(T3Integer(3)) must beFalse
    }
    "remove a non-existing value" in {
      val lookupTable = new LookupTable(T3ObjectId(1), vmState, false, 32, 64)
      lookupTable(T3Integer(3)) = T3Integer(42)
      
      val result = lookupTable.removeElement(T3Integer(5))
      result must_== T3Nil
      lookupTable.entryCount must_== 1
      lookupTable.isKeyPresent(T3Integer(3)) must beTrue
    }
  }
}
