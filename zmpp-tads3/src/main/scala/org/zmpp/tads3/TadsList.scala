/*
 * Created on 2010/10/22
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

import org.zmpp.base._
import scala.collection.JavaConversions._
import java.util.ArrayList

/*
 * Lists are stored in the image as
 * length n (ushort)
 * n * size(DATAHOLDER)
 * Very similar to Vector
 */
class TadsList(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsCollection(id, vmState, isTransient) {
  private val _container = new ArrayList[T3Value]
  override def metaClass: MetaClass = objectSystem.listMetaClass
  override def toString = "List object"
  def size = _container.size
  def initWith(seq: Seq[T3Value]) {
    seq.foreach(value => _container.add(value))
  }

  def addElement(value: T3Value) {
    _container.add(value)
  }
  override def valueAtIndex(index: Int): T3Value = _container(index - 1)
  override def setValueAtIndex(index: Int, newValue: T3Value): T3ObjectId = {
    val oldValue = _container(index - 1)
    _container(index - 1) = newValue
    id // return this object
  }
  def createIterator(argc: Int): T3Value = {
    println("createIterator()")
    val iter = objectSystem.indexedIteratorMetaClass.createIterator(this)
    iter.id
  }
}

class TadsListConstant(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsList(id, vmState, isTransient) {
  override def addElement(value: T3Value) {
    throw new UnsupportedOperationException("can not add to list constant")
  }
  override def setValueAtIndex(index: Int, newValue: T3Value): T3ObjectId = {
    throw new UnsupportedOperationException("can not set value in list constant")
  }
}

class ListMetaClass extends AbstractMetaClass {
  def name = "list"
  override def superMeta = objectSystem.metaClassForName("collection")

  val FunctionVector = Array(undef _, subset _, map _, len _,
                             sublist _, intersect _, indexOf _,
                             car _, cdr _, indexWhich _, forEach _,
                             valWhich _, lastIndexOf _, lastIndexWhich _,
                             lastValWhich _, countOf _, countWhich _,
                             getUnique _, appendUnique _, append _,
                             sort _, prepend _, insertAt _, removeElementAt _,
                             removeRange _, forEachAssoc _)

  def undef(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("undefined")
  }
  def subset(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("subset")
  }
  def map(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("map")
  }
  def len(obj: T3Object, argc: Int): T3Value = {
    new T3Integer(obj.asInstanceOf[TadsList].size)
  }
  def sublist(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("sublist")
  }
  def intersect(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("intersect")
  }
  def indexOf(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("indexOf")
  }
  def car(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("car")
  }
  def cdr(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("cdr")
  }
  def indexWhich(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("indexWhich")
  }
  def forEach(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("forEach")
  }
  def valWhich(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("valWhich")
  }
  def lastIndexOf(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("lastIndexOf")
  }
  def lastIndexWhich(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("lastIndexWhich")
  }
  def lastValWhich(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("lastValWhich")
  }
  def countOf(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("countOf")
  }
  def countWhich(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("countWhich")
  }
  def getUnique(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getUnique")
  }
  def appendUnique(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("appendUnique")
  }
  def append(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("append")
  }
  def sort(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("sort")
  }
  def prepend(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("prepend")
  }
  def insertAt(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("insertAt")
  }
  def removeElementAt(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("removeElementAt")
  }
  def removeRange(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("removeRange")
  }
  def forEachAssoc(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("forEachAssoc")
  }

  def createList(id: T3ObjectId, isTransient: Boolean = false) = {
    new TadsList(id, vmState, isTransient)
  }

  def createListConstant(id: T3ObjectId, offset: T3ListConstant) = {
    import TadsConstants._
    val poolOffset = offset.value
    val len = vmState.image.constantDataShortAt(poolOffset)
    printf("List offset = %s, len: %d\n", offset, len)
    val list = new TadsListConstant(id, vmState, false)
    // TODO initWith()
    for (i <- 0 until len) {
      val valueAddr = poolOffset + 1 + SizeDataHolder * i
      val valueType = vmState.image.constantDataByteAt(valueAddr)
      val value = TypeIds.valueForType(valueType,
                                       vmState.image.constantDataIntAt(
                                         valueAddr + 1))
      list.addElement(T3Value.create(valueType, value))
    }
    list
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
