/*
 * Created on 2010/10/12
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

import scala.collection.JavaConversions._
import java.util.ArrayList
import org.zmpp.base._

// GenericObjects are instances of what the documentation calls "TADS Object".
// We wanted to avoid confusion, because "TadsObject" is the super class of
// all object classes in the ZMPP implementation. "GenericObjects" seems like
// a more fitting name which also more accurately reflects the purpose as
// a very flexible data structure the user can manipulate.
// This implementation currently just delegates to a static object for
// most cases, which is fine for now, but is not pure on-demand loading.
// We won't provide a different approach until for file-based loading,
// because of the need to enumerate all objects with some functions.
// We keep in mind that the generic object has almost a 1:1 representation
// in the load/save image.

// Image format for tads-object instances:
// UINT2 superclass_count
// UINT2 load_image_property_count
// UINT2 flags
// UINT4 superclass_1
// ...
// UINT4 superclass_N
// UINT2 load_image_property_ID_1
// DATAHOLDER load_image_property_value_1
// ...
// UINT2 load_image_property_ID_N
// DATAHOLDER load_image_property_value_N 
class GenericObject(id: TadsObjectId, vmState: TadsVMState,
                    override val isClassObject: Boolean,
                    superClassCount: Int,
                    propertyCount: Int)
extends TadsObject(id, vmState) {
  def metaClass = objectSystem.genericObjectMetaClass

  val superClassIds = new Array[Int](superClassCount)
  val properties    = new Array[Property](propertyCount)
  val extProperties = new ArrayList[Property]

  override def toString = {
    "GenericObject[%s, isClassObject: %b, # super: %d, #props: %d]".format(
      id, isClassObject, superClassCount, propertyCount)
  }
  override def isInstanceOf(obj: TadsObject): Boolean = {
    //printf("GenericObject.isInstanceOf() obj = %s\n", id)
    
    for (superClassId <- superClassIds) {
      //printf("GenericObject.isInstanceOf() super = %d\n", superClassId)
      if (objectSystem.objectWithId(superClassId) == obj) return true
      // TODO: we might have to check whether the super class inherits
      // from obj
    }
    super.isInstanceOf(obj)
  }
  override def getProperty(propertyId: Int, argc: Int):Property = {
    val prop = findPropertyInThis(propertyId)
    if (prop != null) return prop
    // not found in object -> try super class properties
    for (superClassId <- superClassIds) {
      printf("not found, try super class: %d\n", superClassId)
      val superClass = objectSystem.objectWithId(superClassId)
      val prop = superClass.getProperty(propertyId, argc)
      if (prop != null) return prop
    }
    null
  }

  private def findPropertyInThis(propertyId: Int): Property = {
    // intentionally not using find() here
    for (prop <- properties) if (prop.id == propertyId) return prop
    for (prop <- extProperties) if (prop.id == propertyId) return prop
    null
  }

  override def setProperty(propertyId: Int, newValue: TadsValue) {
    val prop = findPropertyInThis(propertyId)
    if (prop == null) {
      printf("prop not found creating new one")
      val newProp = new Property(propertyId, newValue, this.id)
    } else {
      printf("prop found updating existing one")
      prop.tadsValue = newValue
    }
    // TODO: UNDO
  }
}

object GenericObjectMetaClass {
  val FlagIsClass = 0x0001
}
class GenericObjectMetaClass extends MetaClass {
  def name = "tads-object"
  override def createFromImage(objectId: TadsObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): TadsObject = {
    import TadsConstants._
    import GenericObjectMetaClass._
    val superClassCount = imageMem.shortAt(objDataAddr)
    val propertyCount   = imageMem.shortAt(objDataAddr + 2)
    val flags           = imageMem.shortAt(objDataAddr + 4)
    val isClassObject   = (flags & FlagIsClass) == FlagIsClass

    val genericObject = new GenericObject(objectId, vmState,
                                          isClassObject, superClassCount,
                                          propertyCount)
    for (index <- 0 until superClassCount) {
      genericObject.superClassIds(index) = imageMem.shortAt(objDataAddr + 6 +
                                                            index * 4)
    }
    val propertyOffset = objDataAddr + 6 + superClassCount * 4
    for (index <- 0 until propertyCount) {
      val propAddr = propertyOffset + (SizeDataHolder + SizePropertyId) * index
      val propertyId    = imageMem.shortAt(propAddr)
      val propertyType  = imageMem.byteAt(propAddr + 2)
      val propertyValue = TypeIds.valueForType(propertyType,
                                               imageMem.intAt(propAddr + 3))
      genericObject.properties(index) =
        new Property(propertyId,
                     TadsValue.create(propertyType, propertyValue),
                     objectId)
    }
    genericObject
  }
}
