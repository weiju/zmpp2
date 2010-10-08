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

import scala.collection.mutable.HashMap

// The abstract interface to a TADS3 meta class.
trait MetaClass {
  def createFromStack
  def supportsVersion(version: String): Boolean
}

trait Tads3Object {
  def findProperty(propertyId: Int): Property
}

// Define all system meta classes that we know so far
// These are the current meta classes that are provided by the reference
// implementation. Apparently, the technical manual only mentions four of them,
// which makes it hard to implement a VM without looking at the QTads' source code.
// Instead of complaining, I'll just see how they are implemented in QTads and
// document it by myself
class SystemMetaClass extends MetaClass {
  def createFromStack { }
  def supportsVersion(version: String) = true
}
class TadsObjectMetaClass extends SystemMetaClass
class StringMetaClass extends SystemMetaClass
class ListMetaClass   extends SystemMetaClass
class VectorMetaClass extends SystemMetaClass
class LookupTableMetaClass extends SystemMetaClass
class Dictionary2MetaClass extends SystemMetaClass
class GrammarProductionMetaClass extends SystemMetaClass
class AnonFuncPtrMetaClass extends SystemMetaClass
class IntClassModMetaClass extends SystemMetaClass
class RootObjectMetaClass extends SystemMetaClass
class IntrinsicClassMetaClass extends SystemMetaClass
class CollectionMetaClass extends SystemMetaClass
class IteratorMetaClass extends SystemMetaClass
class IndexedIteratorMetaClass extends SystemMetaClass
class CharacterSetMetaClass extends SystemMetaClass
class ByteArrayMetaClass extends SystemMetaClass
class RegexPatternMetaClass extends SystemMetaClass
class WeakRefLookupTableMetaClass extends SystemMetaClass
class LookupTableIteratorMetaClass extends SystemMetaClass
class FileMetaClass extends SystemMetaClass
class StringComparatorMetaClass extends SystemMetaClass
class BigNumberMetaClass extends SystemMetaClass

// A class that wraps the static objects in the load image.
// We might later instead put the static object into a wrapper that
// is associated with the correct meta class
class Tads3StaticObject(objectManager: ObjectManager, staticObject: StaticObject)
extends Tads3Object {
  def findProperty(propertyId: Int): Property = {
    val prop = staticObject.findProperty(propertyId)
    if (prop != null) return prop
    // not found in object try super class properties
    for (i <- 0 until staticObject.superClassCount) {
      val superClassId = staticObject.superClassIdAt(i)
      printf("not found, try super class: %d\n", superClassId)
      val superClass = objectManager.objectWithId(superClassId)
      val prop = superClass.findProperty(propertyId)
      if (prop != null) return prop
    }
    null
  }

  def dump {
    printf("TADS3 OBJECT: %s\n", staticObject.toString)
  }
}

object ObjectSystem {
  // map the unique meta class names to the system meta classes
  // when initializing the game, this map can be used to map the image
  // identifiers for metaclass dependencies to the actual meta classes that
  // the ZMPP TADS3 VM supports
  val MetaClasses = Map(
    "tads-object"          -> new TadsObjectMetaClass,
    "string"               -> new StringMetaClass,
    "list"                 -> new ListMetaClass,
    "vector"               -> new VectorMetaClass,
    "lookuptable"          -> new LookupTableMetaClass,
    "dictionary2"          -> new Dictionary2MetaClass,
    "grammar-production"   -> new GrammarProductionMetaClass,
    "anon-func-ptr"        -> new AnonFuncPtrMetaClass,
    "int-class-mod"        -> new IntClassModMetaClass,
    "root-object"          -> new RootObjectMetaClass,
    "intrinsic-class"      -> new IntrinsicClassMetaClass,
    "collection"           -> new CollectionMetaClass,
    "iterator"             -> new IteratorMetaClass,
    "indexed-iterator"     -> new IndexedIteratorMetaClass,
    "character-set"        -> new CharacterSetMetaClass,
    "bytearray"            -> new ByteArrayMetaClass,
    "regex-pattern"        -> new RegexPatternMetaClass,
    "weakreflookuptable"   -> new WeakRefLookupTableMetaClass,
    "lookuptable-iterator" -> new LookupTableIteratorMetaClass,
    "file"                 -> new FileMetaClass,
    "string-comparator"    -> new StringComparatorMetaClass,
    "bignumber"            -> new BigNumberMetaClass)
}

// The object manager handles instantiation and management of objects
// The overall design philosophy is that objects are only created when
// necessary to support quick serialization/deserialization

class ObjectManager {
  private var _maxObjectId = 0
  private var _image: Tads3Image = null
  private val _objectCache = new HashMap[Int, Tads3Object]
  private val _metaClassMap = new HashMap[Int, MetaClass]

  private def establishMetaClassMapping {
    for (i <- 0 until _image.metaClassDependencies.length) {
      _metaClassMap(i) =
        ObjectSystem.MetaClasses(_image.metaClassDependencies(i).name)
    }
  }

  def connectImage(image: Tads3Image) {
    _maxObjectId = image.maxObjectId
    _image       = image
    establishMetaClassMapping
  }

  def newId = {
    _maxObjectId += 1
    _maxObjectId
  }

  def objectWithId(id: Int): Tads3Object = {
    if (_objectCache.contains(id)) _objectCache(id)
    else {
      // search static objects
      val obj = new Tads3StaticObject(this, _image.staticObjectWithId(id))
      _objectCache(id) = obj
      obj
    }
  }
}
