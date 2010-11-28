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
import scala.collection.JavaConversions._
import org.zmpp.base._

// This file implements the built-in library class/metaclass Vector in TADS3.
// A TADS3 Vector is backed by an ArrayList, so constructor arguments which
// specify sizes will have no effect here.
// Note: Vector indexes in TADS are, as all sequential types in TADS, in the
// range [1..n], and *not* [0..n-1]
class Vector(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsCollection(id, vmState, isTransient) {
  private val _container = new ArrayList[T3Value]

  // because of polymorphism, metaClass can't be relied on to make a
  // static search, so we state the metaClass explicitly
  // this meta class is not inherited, so we can use it in static searches
  private def staticMetaClass: MetaClass = objectSystem.vectorMetaClass
  override def metaClass: MetaClass = objectSystem.vectorMetaClass
  def init(numElements: Int) {
    printf("initialize %d elements\n", numElements)
    for (i <- 0 until numElements) _container.append(T3Nil)
  }

  // for collections, we search the static property list, which is
  // in the object's meta class hierarchy
  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("vector prop idx = %d\n", idx)
    if (idx >= 0) {
      new Property(propertyId,
                   staticMetaClass.callMethodWithIndex(this, idx, argc),
                   id)
    } else super.getProperty(propertyId, argc)
  }

  def size = _container.size
  def append(value: T3Value) = _container.add(value)
  def insertAt(index: Int, value: T3Value) = _container.add(index - 1, value)
  def indexOf(value: T3Value): Int = {
    for (i <- 0 until _container.size) {
      if (_container(i).t3vmEquals(value)) return i + 1
    }
    return 0
  }
  def indexWhich(cond: T3Value): Int = {
    printf("indexWhich(), cond: %s, len = %d\n", cond, size)
    for (i <- 0 until size) {
      vmState.stack.push(_container(i))
      val executor = new Executor(vmState)
      executor.executeCallback(cond, 1)
      // don't forget vector indices are 1-based !!
      if (vmState.r0.isTrue) return (i + 1)
    }
    throw new UnsupportedOperationException("indexWhich() TODO")
  }

  override def valueAtIndex(index: Int): T3Value = _container(index - 1)
  override def setValueAtIndex(index: Int, newValue: T3Value): T3ObjectId = {
    val oldValue = _container(index - 1)
    _container(index - 1) = newValue
    id // return this object
  }
  def createIterator(argc: Int): T3Value = {
    println("vector.createIterator()")
    val iter = objectSystem.indexedIteratorMetaClass.createIterator(this)
    iter.id
  }

  def valWhich(cond: T3Value): T3Value = {
    printf("valWhich(), cond = %s\n", cond)
    if (size == 0) T3Nil
    else throw new UnsupportedOperationException("TODO non-empty vector")
  }

  override def toString = {
    val builder = new StringBuilder("vector(%d elems) [ ".format(_container.size))
    for (elem <- _container) {
      builder.append(elem)
      builder.append(" ")
    }
    builder.append("]")
    builder.toString
  }
  def toList(start: Int, end: Int) = {
    val list = objectSystem.listMetaClass.createList(objectSystem.newObjectId)
    objectSystem.registerObject(list)
    // subList is end index-exclusive
    list.initWith(_container.subList(start - 1, end))
    printf("toList(), start = %d end = %d, list len = %d\n", start, end, list.size)
    list.id
  }
}

// Image format for vector instances:
// UINT2 elements_allocated 
// UINT2 number_of_elements_used
// DATAHOLDER element[1]
// DATAHOLDER element[2]
//
// Object extension: TODO
class VectorMetaClass extends AbstractMetaClass {
  def name = "vector"
  override def superMeta = objectSystem.metaClassForName("collection")

  val FunctionVector = Array(undef _,        toList _,       getSize _,
                             copyFrom _,     fillVal _,      subset _,
                             applyAll _,     indexWhich _,   forEach _,
                             forEachAssoc _, mapAll _,       indexOf _,
                             valWhich _,     lastIndexOf _,  lastIndexWhich _,
                             lastValWhich _, countOf _,      countWhich _,
                             getUnique _,    appendUnique _, sort _,
                             setLength _,    insertAt _,     removeElementAt _,
                             removeRange _,  append _,       prepend _,
                             appendAll _,    removeElement _)

  def undef(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("undefined")
  }
  def toList(obj: T3Object, argc: Int): T3Value = {
    val vector = obj.asInstanceOf[Vector]
    val start = if (argc > 0) vmState.stack.pop.value else 1
    val end   = if (argc > 1) vmState.stack.pop.value else vector.size
    if (argc > 2) throw new UnsupportedOperationException("toList has max 2 params")
    vector.toList(start, end)
  }
  def getSize(obj: T3Object, argc: Int): T3Value = {
    if (argc == 0) {
      new T3Integer(obj.asInstanceOf[Vector].size)
    } else {
      throw new IllegalArgumentException("getSize(): argc must be 0")
    }
  }
  def copyFrom(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("copyFrom")
  }
  def fillVal(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("fillVal")
  }
  def subset(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("subset")
  }
  def applyAll(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("applyAll")
  }
  def indexWhich(obj: T3Object, argc: Int): T3Value = {
    if (argc == 1) {
      val index = obj.asInstanceOf[Vector].indexWhich(vmState.stack.pop)
      if (index == 0) T3Nil else new T3Integer(index)
    } else throw new IllegalArgumentException("wrong arg count: " + argc)
  }
  def forEach(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("indexWhich")
  }
  def forEachAssoc(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("forEach")
  }
  def mapAll(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("mapAll")
  }
  def indexOf(obj: T3Object, argc: Int): T3Value = {
    if (argc == 1) {
      val value = vmState.stack.pop
      val index = obj.asInstanceOf[Vector].indexOf(value)
      printf("vector.indexOf(), argc = %d val = %s index = %d\n", argc, value, index)
      if (index == 0) T3Nil else new T3Integer(index)
    } else throw new IllegalArgumentException("wrong arg count")
  }
  def valWhich(obj: T3Object, argc: Int): T3Value = {
    obj.asInstanceOf[Vector].valWhich(vmState.stack.pop)
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
  def sort(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("sort")
  }
  def setLength(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("setLength")
  }
  def insertAt(obj: T3Object, argc: Int): T3Value = {
    if (argc >= 2) {
      val startIndex = vmState.stack.pop.value
      printf("Vector.insertAt(%d), argc = %d\n", startIndex, argc)
      for (i <- 0 until argc - 1) {
        val value = vmState.stack.pop
        printf("insert at index: %s value: %s\n", startIndex + i, value)
        obj.asInstanceOf[Vector].insertAt(startIndex + i, value)
      }
      obj.id
    } else {
      throw new IllegalArgumentException("at least 2 parameters for insertAt()")
    }
  }
  def removeElementAt(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("removeElementAt")
  }
  def removeRange(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("removeRange")
  }
  def append(obj: T3Object, argc: Int): T3Value = {
    if (argc == 1) {
      val arg = vmState.stack.pop
      printf("obj(%s).append: %s\n", obj, arg)
      obj.asInstanceOf[Vector].append(arg)
      obj.id
    } else {
      throw new IllegalArgumentException("wrong argument count: %d".format(argc))
    }
  }
  def prepend(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("prepend")
  }
  def appendAll(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("appendAll")
  }
  def removeElement(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("removeElement")
  }

  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    val vector = new Vector(objectId, vmState, isTransient)
    // currently, we ignore numAllocated, we might need it for persistence
    val numAllocated = imageMem.shortAt(objDataAddr)
    val numUsed = imageMem.shortAt(objDataAddr + 2)
    printf("Vector::createFromImage(), # alloc: %d, # used: %d\n",
           numAllocated, numUsed)
    // here come numUsed DATAHOLDERs
    for (i <- 0 until numUsed) {
      val currAddr = objDataAddr + 4 + i * DataHolder.Size
      vector.append(T3Value.readDataHolder(imageMem, currAddr))
    }
    vector
  }

  // This is the TADS Vector constructor.
  // Parameters
  // numAllocated (int): number of elements to allocate
  // initParam [optional], type can be
  //   - (int): number of elements to initialize
  //   - (list): copy elements
  //   - (object:vector) copy elements
  // 
  override def createFromStack(id: T3ObjectId, argc: Int,
                               isTransient: Boolean) = {
    if (argc < 1 || argc > 2) {
      throw new IllegalArgumentException("vector::constructor(), argc " +
                                         "must be 1 or 2")
    }
    val numAllocated = vmState.stack.pop
    val result = if (numAllocated.valueType == TypeIds.VmInt) {
      // we ignore this parameter, we do not allocate vectors with an initial size
      new Vector(id, vmState, isTransient)
    } else {
      throw new IllegalArgumentException("vector::constructor(), illegal " +
                                         "numAllocated type")
    }
    // second (optional argument)
    if (argc > 1) {
      val initParam = vmState.stack.pop
      if (initParam.valueType == TypeIds.VmInt) {
        result.init(initParam.value)
      } else {
        throw new UnsupportedOperationException("vector::constructor(), " +
                                                "arg1 type " +
                                                "not yet supported")
      }
    }
    result
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
