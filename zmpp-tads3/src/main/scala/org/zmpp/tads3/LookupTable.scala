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

// Due to the organization of a T3 image file, there is no easy/efficient
// way around implementing our own hash table structure.
// Of all data structures in T3, lookup tables are the data structures that
// are (together with regular expressions) the ones that seem to make the least
// sense in a modern programming platform.
//
// Reasons for this:
// 1. Bucket and value counts are given explicitly in the image file, when
//    we save the image, we will have to preserve it. Providing a bucket
//    and value organization factually requires to provide your own hash
//    table implementation, it might have been more portable to just list
//    key-value pairs.
// 2. The Constant Pool Page blocks can come _after_ the static object block
//    which means at lookup table creation time, we might not have access
//    to the lookup keys yet.
//
// Note: it seems that the image organization not only dictates the hash table
// organization, but also - the hash function itself !!! That's what it seems
// to me at least at the moment.
case class LookupTableEntry(key: T3Value, value: T3Value)

class LookupTable(id: T3ObjectId, vmState: TadsVMState,
                       isTransient: Boolean, bucketCount: Int,
                       initialValueSize: Int)
extends AbstractT3Object(id, vmState, isTransient) {

  val _buckets = new Array[List[LookupTableEntry]](bucketCount)
  for (i <- 0 until bucketCount) _buckets(i) = Nil

  def metaClass = objectSystem.lookupTableMetaClass
  private def staticMetaClass = objectSystem.lookupTableMetaClass

  private def bucketIndexFor(key: T3Value) = {
    val rawHash = if (key.valueType == VmSString) {
      println("calculating hash for sstring")
      objectSystem.stringConstantWithOffset(key.asInstanceOf[T3SString]).hashCode
    } else if (key.valueType == VmEnum || key.valueType == VmInt) {
      println("calculating hash for int/enum")
      key.value
    } else if (key.valueType == VmObj) {
      println("calculating hash for obj: " + objectSystem.objectWithId(key.value))
      objectSystem.objectWithId(key.value).hashCode
    } else {
      throw new UnsupportedOperationException("unsupported hash type")
    }
    printf("bucketIndexFor(key = %s), rawHash = %d, finalHash = %d\n",
           key, rawHash, rawHash % bucketCount)
    rawHash % bucketCount
  }

  // bucketIndex _must_ be 0-based !!!
  def addValueToBucket(bucketIndex: Int, key: T3Value, value: T3Value) {
    _buckets(bucketIndex) = _buckets(bucketIndex) :+ LookupTableEntry(key, value)
  }

  def valuesInBucket(bucketIndex: Int): Seq[LookupTableEntry] = {
    _buckets(bucketIndex - 1).toSeq
  }

  def entryCount = {
    var count = 0
    for (i <- 0 until _buckets.length) count += _buckets(i).length
    count
  }

  def removeElement(key: T3Value): T3Value = {
    val bucketIndex = bucketIndexFor(key)
    val previous = this(key)
    if (previous != T3Nil) {
      _buckets(bucketIndex) = _buckets(bucketIndex).filter(entry =>
        entry.key != key)
    }
    previous
  }

  override def toString = {
    val builder = new StringBuilder
    builder.append("LookupTable [%s] { ".format(id))
    for (bucket <- _buckets; entry <- bucket) {
      builder.append(entry)
      builder.append(",\n")
    }
    builder.append(" }")
    builder.toString
  }

  def forEachAssoc(func: T3Value) {
    printf("forEachAssoc(), func = %s\n", func)
    for (bucket <- _buckets; entry <- bucket) {
      printf("iteration -> (k = %s, v = %s)\n", entry.key, entry.value)
      vmState.stack.push(entry.value)
      vmState.stack.push(entry.key)
      new Executor(vmState).executeCallback(func, 2)
    }
  }
  private def t3vmEquals(value1: T3Value, value2: T3Value) = {
    objectSystem.t3vmEquals(value1, value2)
  }

  def apply(key: T3Value): T3Value = {
    val bucketIndex = bucketIndexFor(key)
    printf("apply(%s)\n", key)
    for (entry <- _buckets(bucketIndex)) {
      printf("compare %s with [%s]\n", entry.key, key)
      if (t3vmEquals(entry.key, key)) return entry.value
    }
    println("not found")
    T3Nil
  }
  def update(key: T3Value, value: T3Value) {
    val bucketIndex = bucketIndexFor(key)
    val listIndex = _buckets(bucketIndex).indexWhere(entry => t3vmEquals(entry.key, key))
    if (listIndex == -1)
      addValueToBucket(bucketIndex, key, value)
    else {
      _buckets(bucketIndex) =
        _buckets(bucketIndex).updated(listIndex,
                                      LookupTableEntry(key, value))
    }
  }
  def keysToList: T3ObjectId = {
    var result: List[T3Value] = Nil
    for (bucket <- _buckets ; entry <- bucket) {
      result = entry.key :: result
    }
    objectSystem.listMetaClass.createList(result).id
  }
  def isKeyPresent(key: T3Value): Boolean = {
    val bucketIndex = bucketIndexFor(key)
    for (entry <- _buckets(bucketIndex)) if (t3vmEquals(entry.key, key)) return true
    false
  }

  override def valueAtIndex(index: T3Value): T3Value = this(index)
  override def setValueAtIndex(index: T3Value, newValue: T3Value): T3ObjectId = {
    this(index) = newValue
    id
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
    val bucketCount    = imageMem.shortAt(objDataAddr)
    val valueCount     = imageMem.shortAt(objDataAddr + 2)
    val firstFreeIndex = imageMem.shortAt(objDataAddr + 4)
    val bucketStart    = objDataAddr + 6
    var valueStart     = bucketStart + 2 * bucketCount
    val valueSize      = 2 * DataHolder.Size + 2

    val lookupTable = new LookupTable(objectId, vmState, isTransient,
                                      bucketCount, valueCount)
    printf("LookupTable::createFromImage(%s), bucketCount: %d, valueCount: %d, firstFreeIndex: %d\n", objectId, bucketCount, valueCount, firstFreeIndex)

    // the only data that is interesting are the actual values in the
    // buckets. If the index is > 0, we build the list of values for
    // the current bucket
    for (bucketIndex <- 0 until bucketCount) {
      var valueIndex = imageMem.shortAt(bucketStart + 2 * bucketIndex)
      while (valueIndex != 0) {
        val valueAddr = valueStart + (valueIndex - 1) * valueSize
        val key       = T3Value.readDataHolder(imageMem, valueAddr)
        val value     = T3Value.readDataHolder(imageMem, valueAddr + DataHolder.Size)
        lookupTable.addValueToBucket(bucketIndex, key, value)
        valueIndex    = imageMem.shortAt(valueAddr + 2 * DataHolder.Size)
      }
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
