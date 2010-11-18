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

// treat Java collections like Scala collections
// until we really understand the object system, we iterate in a similar
// way as the reference
// implementation (by ascending key order), so we need sorted maps.
// unfortunately, the Scala TreeMap is immutable, which might be fine for
// a lot of operations, but in an IF story, we might have frequent updates,
// which are potentially slow with an immutable structure.
// Later, the order will not matter, so we can replace this with a
// Scala HashMap
import scala.collection.JavaConversions._
import java.util.TreeMap
import org.zmpp.base._

// T3 object is the super interface of all objects in the TADS 3 system
trait T3Object {
  def id: T3ObjectId
  def isTransient: Boolean
  def isClassObject : Boolean
  def metaClass: MetaClass
  def isOfMetaClass(meta: MetaClass): Boolean
  def isInstanceOf(obj: T3Object): Boolean
  def getProperty(propertyId: Int, argc: Int): Property
  def setProperty(propertyId: Int, newValue: T3Value)
  def valueAtIndex(index: Int): T3Value
  def setValueAtIndex(index: Int, newValue: T3Value): T3Value
  def inheritProperty(propertyId: Int): T3Value
}

// All classes in the ZMPP TADS3 implementation inherit from
// this abstract base class
abstract class AbstractT3Object(val id: T3ObjectId, val vmState: TadsVMState)
extends T3Object {
  var isTransient = false
  def isClassObject = false
  def metaClass: MetaClass
  def objectSystem = vmState.objectSystem
  
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
  def getProperty(propertyId: Int, argc: Int): Property = {
    throw new UnsupportedOperationException("findProperty() not implemented: " +
                                          getClass.getName)
  }
  def setProperty(propertyId: Int, newValue: T3Value) {
    throw new UnsupportedOperationException("setProperty() not implemented: " +
                                            getClass.getName)
  }
  def valueAtIndex(index: Int): T3Value = {
    throw new UnsupportedOperationException("valueAtIndex() not implemented")
  }
  def setValueAtIndex(index: Int, newValue: T3Value): T3Value = {
    throw new UnsupportedOperationException("setValueAtIndex() not implemented")
  }

  def inheritProperty(propertyId: Int): T3Value = {
    throw new UnsupportedOperationException("inheritProperty() not implemented")
  }
}

// A null object for quick comparison
object InvalidObject extends AbstractT3Object(InvalidObjectId, null) {
  def metaClass = null
}

class Property(val id: Int, var tadsValue: T3Value,
               val definingObject: T3ObjectId) {
  def valueType = tadsValue.valueType
  def value = tadsValue.value
  override def toString = {
    "Property (id = %d value: %s def. obj: %s)".format(
      id, tadsValue, definingObject)
  }
}

// Define all system meta classes that we know so far
// These are the current meta classes that are provided by the reference
// implementation. Apparently, the technical manual only mentions four of
// them, which makes it hard to implement a VM without looking at the
// QTads' source code.
// Instead of complaining, I'll just see how they are implemented in QTads and
// document it by myself
// Meta classes only inherit conceptually from each other, through the superMeta
// relationship, this makes it easier to selectively evaluate static class
// properties
trait MetaClass {
  // After being loaded, its dependency id and the VM state
  // can be directly accessed through its members.
  // This is so each object only needs to store the reference to its
  // meta class, but can still query the system state if necessary
  var id: Int
  var vmState: TadsVMState
  def name: String

  // instead of creating a parallel inheritance hierarchy of meta classes
  // we model the super relationship with aggregation. I think that evaluating
  // class properties feels cleaner this way
  def superMeta: MetaClass
  def reset
  def createFromStack(id: T3ObjectId, argc: Int): T3Object
  def createFromImage(objectId: T3ObjectId, objDataAddr: Int,
                      numBytes: Int, isTransient: Boolean): T3Object
  def supportsVersion(version: String): Boolean
  def callMethodWithIndex(obj: T3Object, index: Int,
                          argc: Int): T3Value
  def evalClassProperty(obj: T3Object, propertyId: Int): T3Value

  // The static property map defines the mapping from a property id to
  // an index into the meta class's function table
  // (property id -> function vector index)
  // it should be cleared when loading a new image to
  // reload the mappings
  def addFunctionMapping(propertyId: Int, functionIndex: Int)
  def functionIndexForProperty(propertyId: Int): Int
}

abstract class AbstractMetaClass extends MetaClass {
  private val propertyMap = new TreeMap[Int, Int]
  def reset = propertyMap.clear
  def superMeta: MetaClass = null
  def createFromStack(id: T3ObjectId,
                      argc: Int): T3Object = {
    throw new UnsupportedOperationException("createFromStack not yet " +
                                            "supported in " +
                                            "metaclass '%s'".format(name))
  }
  def createFromImage(objectId: T3ObjectId, objDataAddr: Int,
                      numBytes: Int, isTransient: Boolean): T3Object = {
    throw new UnsupportedOperationException("createFromImage not yet " +
                                            "supported in " +
                                            "metaclass '%s'".format(name))
  }
  def supportsVersion(version: String) = true

  def callMethodWithIndex(obj: T3Object, index: Int,
                          argc: Int): T3Value = {
    throw new UnsupportedOperationException(
      "%s: callMethodWithIndex not supported".format(name))
  }

  def evalClassProperty(obj: T3Object, propertyId: Int): T3Value = {
    var functionIndex = functionIndexForProperty(propertyId)
    if (functionIndex == -1) {
      if (superMeta != null) superMeta.evalClassProperty(obj, propertyId)
      else T3Nil
    } else {
      // found, try to evaluate
      printf("FOUND PROPERTY %d in metaclass '%s', at index: %d\n",
           propertyId, name, functionIndex)
      callMethodWithIndex(obj, functionIndex, 0)
    }
  }
  var id: Int = 0
  var vmState: TadsVMState = null
  def imageMem = vmState.image.memory
  def objectSystem = vmState.objectSystem
  def addFunctionMapping(propertyId: Int, functionIndex: Int) {
    printf("%s.addFunctionMapping(%d, %d)\n", name, propertyId, functionIndex)
    propertyMap(propertyId) = functionIndex
  }
  def functionIndexForProperty(propertyId: Int) = {
    if (propertyMap.containsKey(propertyId)) propertyMap(propertyId)
    else -1
  }
}

// The top level meta class, the super meta of any other class
class ObjectMetaClass extends AbstractMetaClass {
/*
  val FunctionVector = Array(undef _,          ofKind _,   superClassList _,
                             isPropDefined _,  propType _, propertyList _,
                             propertyParams _, isClass _,  properInherited _,
                             isTransient    _)
*/
  def name = "object"
}

class StringMetaClass extends AbstractMetaClass {
  def name = "string"
}

class IntClassModMetaClass extends AbstractMetaClass {
  def name = "int-class-mod"
}
class CharacterSetMetaClass extends AbstractMetaClass {
  def name = "character-set"
}
class ByteArrayMetaClass extends AbstractMetaClass {
  def name = "bytearray"
}
class WeakRefLookupTableMetaClass extends AbstractMetaClass {
  def name = "weakreflookuptable"
}
class FileMetaClass extends AbstractMetaClass {
  def name = "file"
}

// This is a special meta class that does not do much, its properties can be accessed
// but there is only one root object in the system
class RootObjectMetaClass extends AbstractMetaClass {
  def name = "root-object"
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

// The object manager handles instantiation and management of objects.
// It is an extension of the VM state, because it represents the current state
// of all objects in the system.
// This version of the object manager has to be
// 1. reset before loading a new file
// 2. during loading the image, add metaclass dependencies as they come in
// 3. then load the classes as they come in
class ObjectSystem {
  // map the unique meta class names to the system meta classes
  // when initializing the game, this map can be used to map the image
  // identifiers for metaclass dependencies to the actual meta classes that
  // the ZMPP TADS3 VM supports
  val anonFuncPtrMetaClass         = new AnonFuncPtrMetaClass
  val bigNumberMetaClass           = new BigNumberMetaClass
  val byteArrayMetaClass           = new ByteArrayMetaClass
  val characterSetMetaClass        = new CharacterSetMetaClass
  val collectionMetaClass          = new CollectionMetaClass
  val dictionary2MetaClass         = new Dictionary2MetaClass
  val fileMetaClass                = new FileMetaClass
  val grammarProductionMetaClass   = new GrammarProductionMetaClass
  val indexedIteratorMetaClass     = new IndexedIteratorMetaClass
  val intrinsicClassMetaClass      = new IntrinsicClassMetaClass
  val intClassModMetaClass         = new IntClassModMetaClass
  val iteratorMetaClass            = new IteratorMetaClass
  val listMetaClass                = new ListMetaClass
  val lookupTableMetaClass         = new LookupTableMetaClass
  val lookupTableIteratorMetaClass = new LookupTableIteratorMetaClass
  val objectMetaClass              = new ObjectMetaClass
  val regexPatternMetaClass        = new RegexPatternMetaClass
  val rootObjectMetaClass          = new RootObjectMetaClass
  val stringMetaClass              = new StringMetaClass
  val stringComparatorMetaClass    = new StringComparatorMetaClass
  val tadsObjectMetaClass          = new TadsObjectMetaClass
  val vectorMetaClass              = new VectorMetaClass
  val weakRefLookupTableMetaClass  = new WeakRefLookupTableMetaClass

  val MetaClasses: Map[String, MetaClass] = Map(
    "tads-object"          -> tadsObjectMetaClass,
    "string"               -> stringMetaClass,
    "list"                 -> listMetaClass,
    "vector"               -> vectorMetaClass,
    "lookuptable"          -> lookupTableMetaClass,
    "dictionary2"          -> dictionary2MetaClass,
    "grammar-production"   -> grammarProductionMetaClass,
    "anon-func-ptr"        -> anonFuncPtrMetaClass,
    "int-class-mod"        -> intClassModMetaClass,
    "root-object"          -> rootObjectMetaClass,
    "intrinsic-class"      -> intrinsicClassMetaClass,
    "collection"           -> collectionMetaClass,
    "iterator"             -> iteratorMetaClass,
    "indexed-iterator"     -> indexedIteratorMetaClass,
    "character-set"        -> characterSetMetaClass,
    "bytearray"            -> byteArrayMetaClass,
    "regex-pattern"        -> regexPatternMetaClass,
    "weakreflookuptable"   -> weakRefLookupTableMetaClass,
    "lookuptable-iterator" -> lookupTableIteratorMetaClass,
    "file"                 -> fileMetaClass,
    "string-comparator"    -> stringComparatorMetaClass,
    "bignumber"            -> bigNumberMetaClass)

  private var _maxObjectId       = 0
  private val _objectCache       = new TreeMap[Int, T3Object]
  private val _metaClassMap      = new TreeMap[Int, MetaClass]
  private val _constantCache     = new TreeMap[Int, T3Object] 
  private def image: TadsImage   = vmState.image
  // public member
  var vmState: TadsVMState       = null

  def addMetaClassDependency(metaClassIndex: Int, nameString: String) {
    val name = nameString.split("/")(0)
    val version = if (nameString.split("/").length == 2) nameString.split("/")(1)
                      else "000000"
    _metaClassMap(metaClassIndex) = MetaClasses(name)
    _metaClassMap(metaClassIndex).reset
    _metaClassMap(metaClassIndex).id      = metaClassIndex
    _metaClassMap(metaClassIndex).vmState = vmState
  }

  def addMetaClassPropertyId(metaClassIndex: Int, propertyIndex: Int,
                             propertyId: Int) {
    _metaClassMap(metaClassIndex).addFunctionMapping(propertyId, propertyIndex)
  }
  def addStaticObject(objectId: Int, metaClassIndex: Int,
                      objAddr: Int, numBytes: Int, isTransient: Boolean) {
    val id = new T3ObjectId(objectId)
    val obj = _metaClassMap(metaClassIndex).createFromImage(id, objAddr,
                                                            numBytes, isTransient)
    _objectCache(objectId) = obj
    // set the maximum object id higher so that after we loaded all
    // static objects, we have a start object id
    if (objectId > _maxObjectId) _maxObjectId = objectId + 1
  }

  def reset {
    _maxObjectId = 0
    _metaClassMap.clear
    _objectCache.clear
    _constantCache.clear
  }

  // create a unique object id
  // we assume that a VM runs single-threaded and we never reuse ids
  private def newId = {
    _maxObjectId += 1
    _maxObjectId
  }
  def newObjectId = new T3ObjectId(newId)
  def registerObject(obj: T3Object) {
    _objectCache(obj.id.value) = obj
  }
  def createFromStack(argc: Int, metaClassId: Int) = {
    val id = new T3ObjectId(newId)
    val obj = _metaClassMap(metaClassId).createFromStack(id, argc)
    _objectCache(id.value) = obj
    id
  }

  def printMetaClasses {
    for (i <- _metaClassMap.keys) {
      printf("ID: %d NAME: %s\n", i, _metaClassMap(i).name)
    }
  }

  // **********************************************************************
  // **** Query Functions
  // **********************************************************************

  def metaClassForIndex(index: Int): MetaClass = _metaClassMap(index)
  def metaClassForName(name: String): MetaClass = MetaClasses(name)
  def objectWithId(id: Int): T3Object = {
    if (_objectCache.contains(id)) _objectCache(id)
    else throw new ObjectNotFoundException
  }
  def objectWithId(id: T3Value): T3Object = objectWithId(id.value)
  def listConstantWithOffset(offset: T3ListConstant) = {
    if (_constantCache.containsKey(offset.value)) _constantCache(offset.value)
    else {
      val id = new T3ObjectId(newId)
      val list = listMetaClass.createListConstant(id, offset)
      _objectCache(id.value) = list
      list
    }
  }

  // Enumeration of objects
  def firstObject(enumParams: EnumObjectParams): T3Object = {
    nextObject(InvalidObjectId, enumParams)
  }
  def nextObject(prevObject: T3ObjectId, enumParams: EnumObjectParams): T3Object = {
    var previousFound   = false
    for (entry <- _objectCache) { // entries are pairs of (key, value)
      val currentObj      = entry._2
      var shouldBeChecked = true
      if (!enumParams.enumInstances && !currentObj.isClassObject) shouldBeChecked = false
      if (!enumParams.enumClasses && currentObj.isClassObject) shouldBeChecked = false

      // handle previous object detection
      if (prevObject != InvalidObjectId && !previousFound) shouldBeChecked = false
      if (prevObject != InvalidObjectId && currentObj.id == prevObject) {
        printf("PREVIOUS FOUND !!!!\n")
        previousFound = true
      }
      // TODO: ignore list and string objects
      if (shouldBeChecked) {
        if (enumParams.matchClass == InvalidObject) return currentObj
        if (currentObj.isInstanceOf(enumParams.matchClass)) return currentObj
      }
    }
    InvalidObject
  }
}

class EnumObjectParams(val matchClass: T3Object, val enumInstances: Boolean,
                       val enumClasses: Boolean) {
  override def toString = {
    "EnumObjectParams[matchClass: %s, enumInstances: %b, enumClasses: %b]".format(
      matchClass, enumInstances, enumClasses)
  }
}
