/*
 * Created on 2010/10/29
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

abstract class Iterator(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {
  private val FunctionVector = Array(undef _, getNext _, isNextAvail _, resetIter _,
                                     getCurKey _, getCurVal _)

  def metaClass: MetaClass = objectSystem.iteratorMetaClass

  def undef(argc: Int): T3Value = {
    throw new UnsupportedOperationException("undefined property")
  }

  def getNext(argc: Int): T3Value
  def isNextAvail(argc: Int): T3Value

  def resetIter(argc: Int): T3Value = {
    throw new UnsupportedOperationException("resetIter() not supported")
  }
  def getCurKey(argc: Int): T3Value = {
    throw new UnsupportedOperationException("getCurKey() not supported")
  }
  def getCurVal(argc: Int): T3Value = {
    throw new UnsupportedOperationException("getCurVal() not supported")
  }
  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = objectSystem.iteratorMetaClass.functionIndexForProperty(propertyId)
    new Property(propertyId, FunctionVector(idx)(argc), id)
  }
}

class IndexedIterator(id: T3ObjectId, vmState: TadsVMState,
                      collection: TadsCollection, isTransient: Boolean)
extends Iterator(id, vmState, isTransient) {
  override def metaClass: MetaClass = objectSystem.indexedIteratorMetaClass
  private var currentIndex =  1

  def isNextAvail(argc: Int): T3Value = {
    if (currentIndex <= collection.size) T3True
    else T3Nil
  }
  def getNext(argc: Int): T3Value = {
    val retval = collection.valueAtIndex(T3Integer(currentIndex))
    currentIndex += 1
    retval
  }
}

class LookupTableIteratorMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "lookuptable-iterator"
}

class IteratorMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "iterator"
}

class IndexedIteratorMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "indexed-iterator"
  def createIterator(coll: TadsCollection): IndexedIterator = {
    val id = objectSystem.newObjectId
    val iter = new IndexedIterator(id, vmState, coll, true)
    objectSystem.registerObject(iter)
    iter
  }
}
