/*
 * Created on 2010/10/06
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

import java.util.ArrayList
// treat Java collections like Scala collections
import scala.collection.JavaConversions._

// This file implements the built-in library class/metaclass Vector in TADS3.
// A TADS3 Vector is backed by an ArrayList, so constructor arguments which
// specify sizes will have no effect here.
class Vector extends TadsObject {
  val _container = new ArrayList[Tads3Value]
  def findProperty(propertyId: Int) = null
  def add(value: Tads3Value) {
    _container.add(value)
  }
  def init(numElements: Int) {
    printf("initialize %d elements\n", numElements)
    for (i <- 0 until numElements) _container.add(Tads3Nil)
  }
}

class VectorMetaClass extends SystemMetaClass {
  def name = "vector"

  // This is the TADS Vector constructor.
  // Parameters
  // arg0 (int): number of elements to allocate
  // arg1 [optional], type can be
  //   - (int): number of elements to initialize
  //   - (list): copy elements
  //   - (object:vector) copy elements
  // 
  override def createFromStack(vmState: Tads3VMState, argc: Int) = {
    if (argc < 1 || argc > 2) {
      throw new IllegalArgumentException("vector::constructor(), argc must be 1 or 2")
    }
    val result = new Vector
    val arg0 = vmState.stack.pop
    if (arg0.valueType == TypeIds.VmInt) {
      // just ignore this
      printf("vector::construct(), # elements allocate: %d\n",
             arg0.asInstanceOf[Tads3Integer].value)
    } else {
      throw new IllegalArgumentException("vector::constructor(), illegal arg0 type")
    }
    if (argc > 1) {
      val arg1 = vmState.stack.pop
      if (arg1.valueType == TypeIds.VmInt) {
        result.init(arg1.asInstanceOf[Tads3Integer].value)
      } else {
        throw new UnsupportedOperationException("vector::constructor(), arg1 type " +
                                                "not yet supported")
      }
    }
    result
  }
}
