/*
 * Created on 2010/10/06
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

// All classes in the ZMPP TADS3 implementation inherit from
// this abstract base class. It defines the methods of the RootObject
// class, which every TADS3 object inherits.
abstract class AbstractT3Object(val id: T3ObjectId, val vmState: TadsVMState,
                                val isTransient: Boolean)
extends T3Object {
  import RootObjectMetaClass._

  def isClassObject = false
  def metaClass: MetaClass
  def objectSystem = vmState.objectSystem

  // this was just copied from the reference implementation
  override def hashCode = (id.value & 0xffff) ^ ((id.value & 0xffff0000) >> 16)

  /*
  override def equals(other: Any): Boolean = {
    other match {
      case other:T3Object => this.t3vmEquals(other.id)
      case _              => false
    }
  }
*/
  def isOfMetaClass(meta: MetaClass) = metaClass == meta
  def isInstanceOf(obj: T3Object): Boolean = {
    // the obj parameter needs to be an instance of the IntrinsicClass metaclass
    if (obj.isOfMetaClass(objectSystem.metaClassForName("intrinsic-class"))) {
      // TODO: GET this object's meta class and compare, either if
      // equal or instance
      false
    }
    false
  }

  // The default implementation of object comparison. Only returns true if the
  // object ids are equal
  def t3vmEquals(other: T3Value): Boolean = this.id == other

  def getProperty(propertyId: Int, argc: Int): Property = {
    throw new UnsupportedOperationException("getProperty() not implemented: " +
                                          getClass.getName)
  }
  def setProperty(propertyId: Int, newValue: T3Value) {
    throw new UnsupportedOperationException("setProperty() not implemented: " +
                                            getClass.getName)
  }
  def valueAtIndex(index: T3Value): T3Value = {
    throw new UnsupportedOperationException(
      "%s.valueAtIndex() not implemented".format(metaClass.name))
  }
  def setValueAtIndex(index: T3Value, newValue: T3Value): T3Value = {
    throw new UnsupportedOperationException("setValueAtIndex() not implemented")
  }

  def inheritProperty(propertyId: Int, argc: Int): Property = {
    throw new UnsupportedOperationException("inheritProperty() not implemented")
  }

  def +(other: T3Value): T3Value = {
    throw new UnsupportedOperationException("+() not implemented: " +
                                            getClass.getName)
  }
  def -(other: T3Value): T3Value = {
    throw new UnsupportedOperationException("-() not implemented: " +
                                            getClass.getName)
  }

  // from meta class
  def propInherited(prop: T3PropertyId, origTargetObj: T3ObjectId,
                    definingObj: T3ObjectId, flags: Int): T3Value = {
    // I need to understand the role of origTargetObj and definingObj better
    // For now, we always return T3Nil
/*
    val orig = objectSystem.objectWithId(origTargetObj)
    val inhProp = orig.inheritProperty(prop.value, 0)
    if (inhProp == InvalidProperty) T3Nil
    else throw new UnsupportedOperationException("propInherited() TODO ")
    */
    printf("TODOTODOTODOTODO, RootObject::propInherited()\n")
    T3Nil
  }

  def propDefined(prop: T3PropertyId, flags: Int): T3Value = {
    printf("root-object.propDefined(prop = %s, flags = %d)\n", prop, flags)
    val foundProp = getProperty(prop.value, 0)
    printf("FOUND = %s\n", foundProp)
    if (foundProp != InvalidProperty) {
      flags match {
        case PropDefAny => T3True
        case _ =>
          throw new UnsupportedOperationException("unsupported flags: %d\n".format(flags))
      }
    } else T3Nil
  }
  
  // Since every object in the TADS3 system is a descendant of
  // RootObject, the base implementation is to check whether cls identifies
  // the RootObject intrinsic class
  def ofKind(cls: T3ObjectId): T3Value = {
    val clsObj = objectSystem.objectWithId(cls)
    printf("RootObject::ofKind([%s, %s])\n", cls, clsObj)
    if (clsObj.metaClass == objectSystem.intrinsicClassMetaClass &&
        clsObj.asInstanceOf[IntrinsicClass].representedMetaClass == 
          objectSystem.rootObjectMetaClass) T3True
    else if (cls == this.id) T3True // identity
    else if (isInstanceOf(objectSystem.toT3Object(cls))) T3True
    else T3Nil
  }

  def propType(propId: T3PropertyId): T3Value = {
    val prop = getProperty(propId.value, 0)
    printf("RootObject.propType(%s): prop is: %s\n", propId, prop)
    if (prop == InvalidProperty) return T3Nil
    prop.tadsValue match {
      case _:T3ObjectId =>
        // string objects should be returned as VmSString according to ref
        // list objects as VmList according to ref
        throw new UnsupportedOperationException("TODO propType on objects")
      case _ => T3Integer(prop.valueType)
    }
  }
}

// The top level meta class, the super meta of any other class
// its main responsibility is to define the method that every object has
object RootObjectMetaClass {
  val PropDefAny      = 1
  val PropDefDirectly = 2
  val PropDefInherits = 3
  val PropDefGetClass = 4
}
class RootObjectMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  import RootObjectMetaClass._

  val FunctionVector = Array(undef _,          ofKind _,   getSuperClassList _,
                             propDefined _,    propType _, getPropList _,
                             getPropParams _,  isClass _,  propInherited _,
                             isTransient _)

  def name = "root-object"

  def undef(obj: T3Object, argc: Int): T3Value = {
    // we did not find a native bytecode implementation for the static property
    // We do the same as the reference implementation and look for a modifier
    // property with defining object InvalidObjectId and target object 'self'
    // For now, we always return InvalidPropertyId until we hit a problem
    InvalidPropertyId
  }
  def ofKind(obj: T3Object, argc: Int): T3Value = {
    argc must_== 1
    val isInstance = obj.ofKind(vmState.stack.pop.asInstanceOf[T3ObjectId])
    printf("%s.ofKind() = %s\n", obj, isInstance)
    isInstance
  }
  def getSuperClassList(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getSuperClassList")
  }
  def propDefined(obj: T3Object, argc: Int): T3Value = {
    argc mustBeInRange(1, 2)
    val prop = vmState.stack.pop.asInstanceOf[T3PropertyId]
    val flags = if (argc == 2) vmState.stack.pop.value else PropDefAny
    obj.propDefined(prop, flags)
  }
  def propType(obj: T3Object, argc: Int): T3Value = {
    argc must_== 1
    obj.propType(vmState.stack.pop.asInstanceOf[T3PropertyId])
  }
  def getPropList(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getPropList")
  }
  def getPropParams(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getPropParams")
  }
  def isClass(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isClass")
  }
  def propInherited(obj: T3Object, argc: Int): T3Value = {
    argc mustBeInRange(3, 4)
    val prop          = vmState.stack.pop
    val origTargetObj = vmState.stack.pop
    val definingObj   = vmState.stack.pop
    val flags = if (argc == 4) vmState.stack.pop.value else PropDefAny
    printf("propInherited(%d), prop = %s origTrgObj = %s defObj = %s, flags = %s\n",
           argc, prop, origTargetObj, definingObj, flags)
    obj.propInherited(prop.asInstanceOf[T3PropertyId],
                      origTargetObj.asInstanceOf[T3ObjectId],
                      definingObj.asInstanceOf[T3ObjectId], flags)
  }
  def isTransient(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isTransient")
  }
  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
