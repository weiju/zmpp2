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

trait TadsObject {
  def isTransient: Boolean
  def findProperty(propertyId: Int): Property
  def valueAtIndex(index: Int): Tads3Value
  def setValueAtIndex(index: Int, newValue: Tads3Value): Tads3Value
}

abstract class AbstractTadsObject extends TadsObject {
  def isTransient = false
  def findProperty(propertyId: Int) = {
    throw new UnsupportedOperationException("findProperty() not implemented")
  }
  def valueAtIndex(index: Int) = {
    throw new UnsupportedOperationException("valueAtIndex() not implemented")
  }
  def setValueAtIndex(index: Int, newValue: Tads3Value) = {
    throw new UnsupportedOperationException("setValueAtIndex() not implemented")
  }
}


// The abstract interface to a TADS3 meta class.
trait MetaClass {
  def name: String
  def createFromStack(vmState: Tads3VMState, argc: Int): TadsObject
  def supportsVersion(version: String): Boolean
}

// Define all system meta classes that we know so far
// These are the current meta classes that are provided by the reference
// implementation. Apparently, the technical manual only mentions four of them,
// which makes it hard to implement a VM without looking at the QTads' source code.
// Instead of complaining, I'll just see how they are implemented in QTads and
// document it by myself
abstract class SystemMetaClass extends MetaClass {
  def createFromStack(vmState: Tads3VMState, argc: Int): TadsObject = {
    throw new UnsupportedOperationException("createFromStack not yet supported in " +
                                            "metaclass '%s'".format(name))
  }
  def supportsVersion(version: String) = true
}
class TadsObjectMetaClass extends SystemMetaClass {
  def name = "tads-object"
}
class StringMetaClass extends SystemMetaClass {
  def name = "string"
}
class ListMetaClass   extends SystemMetaClass {
  def name = "list"
}
class LookupTableMetaClass extends SystemMetaClass {
  def name = "lookuptable"
}
class Dictionary2MetaClass extends SystemMetaClass {
  def name = "dictionary2"
}
class GrammarProductionMetaClass extends SystemMetaClass {
  def name = "grammar-production"
}

class IntClassModMetaClass extends SystemMetaClass {
  def name = "int-class-mod"
}
class RootObjectMetaClass extends SystemMetaClass {
  def name = "root-object"
}
class IntrinsicClassMetaClass extends SystemMetaClass {
  def name = "intrinsic-class"
}
class CollectionMetaClass extends SystemMetaClass {
  def name = "collection"
}
class IteratorMetaClass extends SystemMetaClass {
  def name = "iterator"
}
class IndexedIteratorMetaClass extends SystemMetaClass {
  def name = "indexed-iterator"
}
class CharacterSetMetaClass extends SystemMetaClass {
  def name = "character-set"
}
class ByteArrayMetaClass extends SystemMetaClass {
  def name = "bytearray"
}
class RegexPatternMetaClass extends SystemMetaClass {
  def name = "regex-pattern"
}
class WeakRefLookupTableMetaClass extends SystemMetaClass {
  def name = "weakreflookuptable"
}
class LookupTableIteratorMetaClass extends SystemMetaClass {
  def name = "lookuptable-iterator"
}
class FileMetaClass extends SystemMetaClass {
  def name = "file"
}
class StringComparatorMetaClass extends SystemMetaClass {
  def name = "string-comparator"
}
class BigNumberMetaClass extends SystemMetaClass {
  def name = "bignumber"
}

// A class that wraps the static objects in the load image.
// We might later instead put the static object into a wrapper that
// is associated with the correct meta class
class Tads3StaticObject(objectManager: ObjectManager, staticObject: StaticObject)
extends TadsObject {
  val isTransient = staticObject.isTransient

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

  def valueAtIndex(index: Int) = throw new CannotIndexTypeException
  def setValueAtIndex(index: Int, newValue: Tads3Value) = {
    throw new CannotIndexTypeException
  }
  override def toString = staticObject.toString
}

// Predefined symbols that the image defines. Can be accessed by the VM through
// the unique name
object PredefinedSymbols {
  val Constructor             = "Constructor"
  val Destructor              = "Destructor"
  val ExceptionMessage        = "exceptionMessage"
  val FileNotFoundException   = "File.FileNotFoundException"
  val FileCreationExcetpion   = "File.FileCreationException"
  val FileOpenException       = "File.FileOpenException"
  val FileIOException         = "File.FileIOException"
  val FileSyncException       = "File.FileSyncException"
  val FileClosedException     = "File.FileClosedException"
  val FileModeException       = "File.FileModeException"
  val FileSafetyException     = "File.FileSafetyException"
  val GpFirstTokenIndex       = "GrammarProd.firstTokenIndex"
  val GpLastTokenIndex        = "GrammarProd.lastTokenIndex"
  val GpTokenList             = "GrammarProd.tokenList"
  val GpTokenMatchList        = "GrammarProd.tokenMatchList"
  val GpGrammarAltInfo        = "GrammarProd.GrammarAltInfo"
  val GpGrammarAltTokInfo     = "GrammarProd.GrammarAltTokInfo"
  val IfcCalcHash             = "IfcComparator.calcHash"
  val IfcMatchValues          = "IfcComparator.matchValues"
  val LastProp                = "LastProp"
  val MainRestore             = "mainRestore"
  val ObjectCallProp          = "ObjectCallProp"
  val PropNotDefined          = "propNotDefined"
  val RuntimeError            = "RuntimeError"
  val T3StackInfo             = "T3StackInfo"
  val UnknownCharSetException = "CharacterSet.UnknownCharSetException"
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
//
class ObjectManager {
  private var _maxObjectId       = 0
  private val _objectCache       = new HashMap[Int, TadsObject]
  private val _metaClassMap      = new HashMap[Int, MetaClass]
  private var _vmState: Tads3VMState = null

  private def image : Tads3Image = _vmState.image
  private def metaClassDependencies = _vmState.image.metaClassDependencies
  private def establishMetaClassMapping {
    for (i <- 0 until metaClassDependencies.length) {
      _metaClassMap(i) =
        ObjectSystem.MetaClasses(metaClassDependencies(i).name)
      // TODO: map properties as well
    }
  }

  def reset(vmState: Tads3VMState) {
    _vmState = vmState
    _metaClassMap.clear
    _objectCache.clear
    _maxObjectId = image.maxObjectId
    establishMetaClassMapping
  }

  // create a unique object id
  // we assume that a VM runs single-threaded and we never reuse ids
  private def newId = {
    _maxObjectId += 1
    _maxObjectId
  }

  def objectWithId(id: Int) = {
    if (_objectCache.contains(id)) _objectCache(id)
    else {
      // search static objects
      val obj = new Tads3StaticObject(this, image.staticObjectWithId(id))
      _objectCache(id) = obj
      obj
    }
  }

  def createFromStack(argc: Int, metaClassId: Int) = {
    val id = newId
    val obj = _metaClassMap(metaClassId).createFromStack(_vmState, argc)
    _objectCache(id) = obj
    //printf("CREATED OBJECT WITH ID: %d\n", id)
    new Tads3ObjectId(id)
  }
}
