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
class TadsList(id: TadsObjectId, vmState: TadsVMState)
extends TadsCollection(id, vmState) {
  private val _container = new ArrayList[TadsValue]

  override def metaClass: MetaClass = objectSystem.listMetaClass
  override def toString = "List object"
  def size = _container.size
  def addElement(value: TadsValue) {
    _container.add(value)
  }
  override def valueAtIndex(index: Int): TadsValue = _container(index - 1)
  override def setValueAtIndex(index: Int, newValue: TadsValue): TadsObjectId = {
    val oldValue = _container(index - 1)
    _container(index - 1) = newValue
    id // return this object
  }
  def createIterator(argc: Int): TadsValue = {
    println("createIterator()")
    val iter = objectSystem.indexedIteratorMetaClass.createIterator(this)
    iter.id
  }
}

class ListMetaClass extends MetaClass {
  def name = "list"
  override def superMeta = objectSystem.metaClassForName("collection")
  def createListConstant(id: TadsObjectId, offset: TadsListConstant) = {
    import TadsConstants._
    val poolOffset = offset.value
    val len = vmState.image.constantDataShortAt(poolOffset)
    printf("List offset = %s, len: %d\n", offset, len)
    val list = new TadsList(id, vmState)
    for (i <- 0 until len) {
      val valueAddr = poolOffset + 1 + SizeDataHolder * i
      val valueType = vmState.image.constantDataByteAt(valueAddr)
      val value = TypeIds.valueForType(valueType,
                                       vmState.image.constantDataIntAt(
                                         valueAddr + 1))
      list.addElement(TadsValue.create(valueType, value))
    }
    list
  }
}
