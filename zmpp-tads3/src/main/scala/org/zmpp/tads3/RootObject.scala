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

// All classes in the ZMPP TADS3 implementation inherit from
// this abstract base class. It defines the methods of the RootObject
// class, which every TADS3 object inherits.
abstract class AbstractT3Object(val id: T3ObjectId, val vmState: TadsVMState,
                                val isTransient: Boolean)
extends T3Object {
  def isClassObject = false
  def metaClass: MetaClass
  def objectSystem = vmState.objectSystem
  override def hashCode = id.hashCode
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
  def t3vmEquals(other: T3Value): Boolean = {
    printf("t3vmEquals(), this is: %s other is: %s\n", this, other)
    throw new UnsupportedOperationException("t3vmEquals() - TODO")
  }
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

  def +(other: T3Object): T3Object = {
    throw new UnsupportedOperationException("+() not implemented")
  }
}

// The top level meta class, the super meta of any other class
// its main responsibility is to define the method that every object has
class RootObjectMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  val FunctionVector = Array(undef _,          ofKind _,   getSuperClassList _,
                             propDefined _,    propType _, getPropList _,
                             getPropParams _,  isClass _,  propInherited _,
                             isTransient _)

  def name = "root-object"

  def undef(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("undefined")
  }
  def ofKind(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("ofKind")
  }
  def getSuperClassList(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getSuperClassList")
  }
  def propDefined(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("propDefined")
  }
  def propType(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("propType")
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
    throw new UnsupportedOperationException("propInherited")
  }
  def isTransient(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isTransient")
  }
  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
