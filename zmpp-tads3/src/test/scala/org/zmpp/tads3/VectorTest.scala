/*
 * Created on 2010/11/07
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
package org.zmpp.tads3

import org.specs._
import org.specs.runner.{ConsoleRunner, JUnit4}

class VectorTest extends JUnit4(VectorSpec)
object VectorSpecRunner extends ConsoleRunner(VectorSpec)

object VectorSpec extends Specification {
  var objectSystem : ObjectSystem = null
  var vmState : TadsVMState = null

  "Vector" should {
    doBefore {
      objectSystem = new ObjectSystem
      vmState = new TadsVMState(objectSystem)
    }
    "be created" in {
      val vector = new Vector(new T3ObjectId(1), vmState, false)
      vector.metaClass.name must_== "vector"
      vector.size must_== 0
    }
    "append an element" in {
      val vector = new Vector(new T3ObjectId(1), vmState, false)
      val value = new T3Integer(4711)
      vector.append(value)
      vector.size must_== 1
      vector.valueAtIndex(1) must_== value
    }
    "insert an element at position 1" in {
      val vector = new Vector(new T3ObjectId(1), vmState, false)
      val value0 = new T3Integer(0)
      val value1 = new T3Integer(1)
      vector.append(value0)
      vector.insertAt(1, value1)
      vector.size must_== 2
      vector.valueAtIndex(1) must_== value1
      vector.valueAtIndex(2) must_== value0
    }
    "insert an element at the end" in {
      val vector = new Vector(new T3ObjectId(1), vmState, false)
      val value0 = new T3Integer(0)
      val value1 = new T3Integer(1)
      vector.append(value0)
      vector.insertAt(2, value1)
      vector.size must_== 2
      vector.valueAtIndex(1) must_== value0
      vector.valueAtIndex(2) must_== value1
    }
  }
}
