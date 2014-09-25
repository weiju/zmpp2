/*
 * Created on 2010/10/14
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

import T3Assert._

class CharacterSet(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean,
                   charsetName: String)
extends AbstractT3Object(id, vmState, isTransient) {
  protected def staticMetaClass = objectSystem.characterSetMetaClass
  def metaClass = objectSystem.characterSetMetaClass

  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("collection prop idx = %d\n", idx)
    val prop = staticMetaClass.callMethodWithIndex(this, idx, argc)
    if (prop != InvalidPropertyId) new Property(propertyId, prop, id)
    else super.getProperty(propertyId, argc)
  }
}

class CharacterSetMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "character-set"
  val FunctionVector = Array(undef _,      getName _,             isMappingKnown _,
                             isMappable _, isRoundTripMappable _)

  def undef(obj: T3Object, argc: Int): T3Value = InvalidPropertyId
  def getName(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getName")
  }
  def isMappingKnown(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isMappingKnown")
  }
  def isMappable(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isMappable")
  }
  def isRoundTripMappable(obj: T3Object, argc: Int): T3Value = {
    argc must_== 1
    vmState.stack.pop // pop the argument
    T3True
  }

  override def createFromStack(id: T3ObjectId, argc: Int,
                               isTransient: Boolean) = {
    argc must_== 1
    val charsetName = objectSystem.toTadsString(vmState.stack.pop).string
    new CharacterSet(id, vmState, isTransient, charsetName)
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
