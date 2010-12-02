/*
 * Created on 2010/10/13
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

import java.util.HashMap
import scala.collection.JavaConversions._
import org.zmpp.base._
import TypeIds._

class LookupTable(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {
  val _container = new HashMap[Int, T3Value]
  def metaClass = objectSystem.lookupTableMetaClass
  // implements "array-like" access semantics
  def apply(key: T3Value): T3Value = _container(makeHash(key))
  def update(key: T3Value, value: T3Value) {
    _container(makeHash(key)) = value
  }
  private def makeHash(key: T3Value) = {
    if (key.valueType == VmSString) {
      objectSystem.stringConstantWithOffset(key.asInstanceOf[T3SString]).hashCode
    } else if (key.valueType == VmEnum) {
      key.value
    } else {
      throw new UnsupportedOperationException("unsupported hash type")
    }
  }

  override def valueAtIndex(index: Int): T3Value = {
    if (_container.contains(index)) _container(index) else T3Nil
  }
}

class LookupTableMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "lookuptable"
  override def superMeta = objectSystem.metaClassForName("collection")
  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    val lookupTable = new LookupTable(objectId, vmState, isTransient)
    val bucketCount = imageMem.shortAt(objDataAddr)
    val valueCount = imageMem.shortAt(objDataAddr)
    val firstFreeIndex = imageMem.shortAt(objDataAddr)
    printf("LookupTable::createFromImage(), bucketCount: %d, valueCount: %d, firstFreeIndex: %d\n", bucketCount, valueCount, firstFreeIndex)
    val bucketStart = objDataAddr + 6
    for (i <- 0 until bucketCount) {
      val bucketIndex = imageMem.shortAt(bucketStart + 2 * i)
      printf("bucket_index[%d] = %d\n", i, bucketIndex)
    }
    var valueAddr = bucketStart + 2 * bucketCount
    val valueSize = 2 * DataHolder.Size + 2
    for (i <- 0 until valueCount) {
      val key = T3Value.readDataHolder(imageMem, valueAddr)
      val value = T3Value.readDataHolder(imageMem, valueAddr + DataHolder.Size)
      val nextIndex = imageMem.shortAt(valueAddr + 2 * DataHolder.Size)
      printf("value[%d] = {k: %s, v: %s, nextIdx: %d}\n", i, key, value, nextIndex)
      if (key != T3Empty) lookupTable(key) = value
      valueAddr += valueSize
    }
    lookupTable
  }
}
