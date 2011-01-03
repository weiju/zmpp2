/*
 * Created on 2010/10/13
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

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import org.zmpp.base._
import TypeIds._
import T3Assert._

class LookupTable(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean,
                  bucketCount: Int, initialValueSize: Int)
extends AbstractT3Object(id, vmState, isTransient) {
  // we store hash keys and the values in separate collections,
  // since we can not simply call hashCode() on every T3Value
  // TODO: this is not entirely water-proof. Different values might be
  // mapping to the same hashcode.
  val _keys = new HashSet[T3Value]
  val _container = new HashMap[Int, T3Value]

  def metaClass = objectSystem.lookupTableMetaClass
  private def staticMetaClass = objectSystem.lookupTableMetaClass

  def entryCount = _keys.size

  private def isStringOrObject(key: T3Value) = {
    key.valueType == VmSString || key.valueType == VmObj
  }

  def isKeyPresent(key: T3Value) = {
    // we need to check whether the key is a string constant or a reference
    // to a string object
    if (isStringOrObject(key)) _container.contains(makeHash(key))
    else _keys.contains(key)
  }
  // implements "array-like" access semantics
  def apply(key: T3Value): T3Value = {
    _container(makeHash(key))
  }
  def update(key: T3Value, value: T3Value) {
    if (!isKeyPresent(key)) _keys += key
    _container(makeHash(key)) = value
  }
  def removeElement(key: T3Value): T3Value = {
    if (isKeyPresent(key)) {
      val result = this(key)
      _keys -= key
      _container -= makeHash(key)
      result
    } else T3Nil
  }
  def forEachAssoc(func: T3Value) {
    printf("forEachAssoc(), func = %s\n", func)
    _keys.foreach(key => {
      printf("iteration -> (k = %s, v = %s)\n", key, this(key))
      vmState.stack.push(this(key))
      vmState.stack.push(key)
      new Executor(vmState).executeCallback(func, 2)
    })
  }

  def keysToList: T3ObjectId = {
    objectSystem.listMetaClass.createList(_keys.toSeq).id
  }

  private def makeHash(key: T3Value) = {
    if (key.valueType == VmSString) {
      objectSystem.stringConstantWithOffset(key.asInstanceOf[T3SString]).hashCode
    } else if (key.valueType == VmEnum || key.valueType == VmInt) {
      key.value
    } else if (key.valueType == VmObj) {
      objectSystem.objectWithId(key.value).hashCode
    } else {
      throw new UnsupportedOperationException("unsupported hash type")
    }
  }

  override def valueAtIndex(index: T3Value): T3Value = {
    if (isKeyPresent(index)) this(index) else T3Nil
  }
  override def setValueAtIndex(index: T3Value, newValue: T3Value): T3ObjectId = {
    this(index) = newValue
    id // return this object
  }

  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("lookup-table prop idx = %d\n", idx)
    val prop = staticMetaClass.callMethodWithIndex(this, idx, argc)
    if (prop != InvalidPropertyId) new Property(propertyId, prop, id)
    else super.getProperty(propertyId, argc)
  }
}

class LookupTableMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "lookuptable"
  override def superMeta = objectSystem.collectionMetaClass

  val FunctionVector = Array(undef _, isKeyPresent _, removeElement _,
                             applyAll _, forEach _, getBucketCount _,
                             getEntryCount _, forEachAssoc _,
                             keysToList _, valsToList _)

  def undef(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("undefined")
  }
  def isKeyPresent(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isKeyPresent")
  }
  def removeElement(obj: T3Object, argc: Int): T3Value = {
    argc must_== 1
    obj.asInstanceOf[LookupTable].removeElement(vmState.stack.pop)
  }
  def applyAll(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("applyAll")
  }
  def forEach(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("forEach")
  }
  def getBucketCount(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getBucketCount")
  }
  def getEntryCount(obj: T3Object, argc: Int): T3Value = {
    argc must_== 0
    T3Integer(obj.asInstanceOf[LookupTable].entryCount)
  }
  def forEachAssoc(obj: T3Object, argc: Int): T3Value = {
    argc must_== 1
    obj.asInstanceOf[LookupTable].forEachAssoc(vmState.stack.pop)
    T3Nil
  }
  def keysToList(obj: T3Object, argc: Int): T3Value = {
    argc must_== 0
    obj.asInstanceOf[LookupTable].keysToList
  }
  def valsToList(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("valsToList")
  }

  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    val bucketCount = imageMem.shortAt(objDataAddr)
    val valueCount = imageMem.shortAt(objDataAddr)
    val lookupTable = new LookupTable(objectId, vmState, isTransient,
                                      bucketCount, valueCount)
    val firstFreeIndex = imageMem.shortAt(objDataAddr)
    //printf("LookupTable::createFromImage(), bucketCount: %d, valueCount: %d, firstFreeIndex: %d\n", bucketCount, valueCount, firstFreeIndex)
    val bucketStart = objDataAddr + 6
    for (i <- 0 until bucketCount) {
      val bucketIndex = imageMem.shortAt(bucketStart + 2 * i)
      //printf("bucket_index[%d] = %d\n", i, bucketIndex)
    }
    var valueAddr = bucketStart + 2 * bucketCount
    val valueSize = 2 * DataHolder.Size + 2
    for (i <- 0 until valueCount) {
      val key = T3Value.readDataHolder(imageMem, valueAddr)
      val value = T3Value.readDataHolder(imageMem, valueAddr + DataHolder.Size)
      val nextIndex = imageMem.shortAt(valueAddr + 2 * DataHolder.Size)
      //  printf("value[%d] = {k: %s, v: %s, nextIdx: %d}\n", i, key, value, nextIndex)
      if (key != T3Empty) lookupTable(key) = value
      valueAddr += valueSize
    }
    lookupTable
  }

  override def createFromStack(id: T3ObjectId, argc: Int,
                               isTransient: Boolean) = {
    var bucketCount  = 32
    var initCapacity = 64
    if (argc == 2) {
      bucketCount  = vmState.stack.pop.value
      initCapacity = vmState.stack.pop.value
    } else if (argc != 0) {
      throw new IllegalArgumentException("wrong # of arguments")
    }
    new LookupTable(id, vmState, isTransient, bucketCount, initCapacity)
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
