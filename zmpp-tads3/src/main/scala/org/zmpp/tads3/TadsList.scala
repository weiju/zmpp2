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
import T3Assert._
import TypeIds._

/*
 * Lists are stored in the image as
 * length n (ushort)
 * n * size(DATAHOLDER)
 * Very similar to Vector
 */
class TadsList(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsCollection(id, vmState, isTransient) {
  private val _container = new ArrayList[T3Value]
  private def staticMetaClass = objectSystem.listMetaClass
  override def metaClass: MetaClass = objectSystem.listMetaClass
  override def toString = "List object"
  def size = _container.size
  def reverseSeq: Seq[T3Value] = _container.reverse 
  def initWith(seq: Seq[T3Value]) {
    seq.foreach(value => _container.add(value))
  }

  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("list prop idx = %d\n", idx)
    if (idx >= 0) {
      new Property(propertyId,
                   staticMetaClass.callMethodWithIndex(this, idx, argc),
                   id)
    } else super.getProperty(propertyId, argc)
  }

  def addElement(value: T3Value) {
    _container.add(value)
  }
  override def valueAtIndex(index: T3Value): T3Value = _container(index.value - 1)
  override def setValueAtIndex(index: T3Value, newValue: T3Value): T3ObjectId = {
    val oldValue = _container(index.value - 1)
    _container(index.value - 1) = newValue
    id // return this object
  }
  def indexOf(value: T3Value): T3Value = {
    val index = _container.indexOf(value)
    if (index < 0) T3Nil else new T3Integer(index + 1)
  }
  def createIterator(argc: Int): T3Value = {
    println("createIterator()")
    val iter = objectSystem.indexedIteratorMetaClass.createIterator(this)
    iter.id
  }

  override def hashCode: Int = {
    var result = 0
    for (elem <- _container) {
      result += elem.hashCode
    }
    result
  }

  def valWhich(cond: T3Value): T3Value = {
    printf("TadsList::valWhich(cond = %s)[%s]\n", cond, this)
    for (i <- 0 until size) {
      printf("TadsList::valWhich(), 0-index = %d, value = %s\n", i, _container(i))
      vmState.stack.push(_container(i))
      new Executor(vmState).executeCallback(cond, 1)
      if (vmState.r0.isTrue) return _container(i)
    }
    T3Nil
  }
  def subset(func: T3Value): T3Value = {
    printf("subset(f = %s)\n", func)
    var result: List[T3Value] = Nil 
    for (i <- 0 until size) {
      printf("TadsList::subset(), 0-index = %d, value = %s\n", i, _container(i))
      vmState.stack.push(_container(i))
      new Executor(vmState).executeCallback(func, 1)
      if (vmState.r0.isTrue) result = _container(i) :: result
    }
    staticMetaClass.createList(result.reverse, true).id
  }
}

class TadsListConstant(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsList(id, vmState, isTransient) {
  override def addElement(value: T3Value) {
    throw new UnsupportedOperationException("can not add to list constant")
  }
  override def setValueAtIndex(index: T3Value, newValue: T3Value): T3ObjectId = {
    throw new UnsupportedOperationException("can not set value in list constant")
  }
}

class ListMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "list"
  override def superMeta = objectSystem.collectionMetaClass

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
    argCountMustBe(argc, 1)
    obj.asInstanceOf[TadsList].subset(vmState.stack.pop)
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
    argCountMustBe(argc, 1)
    obj.asInstanceOf[TadsList].indexOf(vmState.stack.pop)
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
    argCountMustBe(argc, 1)
    val result = obj.asInstanceOf[TadsList].valWhich(vmState.stack.pop)
    printf("TadsList::valWhich() RESULT = %s\n", result)
    result
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

  def createList(seq: Seq[T3Value], isTransient: Boolean = false) = {
    val list = new TadsList(objectSystem.newObjectId, vmState, isTransient)
    list.initWith(seq)
    objectSystem.registerObject(list)
    list
  }

  override def createFromStack(id: T3ObjectId, argc: Int,
                      isTransient: Boolean): T3Object = {
    printf("TadsList::createFromStack(), argc = %d\n", argc)
    val list = new TadsList(id, vmState, isTransient)
    for (i <- 0 until argc) {
      list.addElement(vmState.stack.pop)
    }
    list
  }

  def createFromParams(fixedArgCount: Int, isTransient: Boolean): T3Object = {
    val numToPush = vmState.getArgc.value - fixedArgCount
    printf("TadsList::createFromParams(%d), argc = %d, to push: %d\n",
           fixedArgCount, vmState.getArgc.value, numToPush)
    val list = new TadsList(objectSystem.newObjectId, vmState, isTransient)
    objectSystem.registerObject(list)
    for (i <- 0 until numToPush) {
      list.addElement(vmState.getArg(fixedArgCount + i))
    }
    list
  }

  def createListConstant(offset: T3ListConstant) = {
    import TadsConstants._
    val poolOffset = offset.value
    val len = vmState.image.constantDataShortAt(poolOffset)
    printf("List offset = %s, len: %d\n", offset, len)
    val list = new TadsListConstant(objectSystem.newObjectId, vmState, false)
    objectSystem.registerObject(list)
    objectSystem.registerConstant(offset, list)
    val initList = new ArrayList[T3Value]

    for (i <- 0 until len) {
      val valueAddr = poolOffset + 2 + DataHolder.Size * i
      val valueType = vmState.image.constantDataByteAt(valueAddr)
      // in the image, empty entries can be represented with a valueType of 0
      // which is an invalid value type. We follow the reference implementation
      // and create Nil entries for that case
      val value = if (valueType == 0) T3Nil
                  else {
                    T3Value.create(valueType,
                                   DataHolder.valueForType(valueType,
                                                           vmState.image.constantDataIntAt(
                                                             valueAddr + 1)))
                  }
      initList.add(value)
    }
    list.initWith(initList)
    list
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
