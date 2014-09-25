/*
 * Created on 2010/10/13
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

import scala.collection.mutable.HashMap
import org.zmpp.base._
import T3Assert._

// The image file data block is arranged as follows:
  
// UINT4 comparator_object_id
// UINT2 load_image_entry_count
// entry 1
// entry 2
// ...
// entry N

// Each entry has the following structure:

// UCHAR key_string_byte_length
// key_string (UTF-8 characters, not null terminated, XOR'ed with 0xBD)
// UINT2 number of sub-entries
// sub-entry 1
// sub-entry 2
// etc

// Each sub-entry is structured like this:

// UINT4 associated_object_id
// UINT2 defining_property_id

// Note that each byte of the key string is XOR'ed with the arbitrary
// byte value 0xBD.  This is simply to provide a minimal level of
// obfuscation in the image file to prevent casual browsing of the image
// contents.

case class DictionarySubEntry(val associatedObjectId: Int, val definingPropertyId: Int)

class Dictionary2(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean,
                  val comparatorObjectId: T3ObjectId)
extends AbstractT3Object(id, vmState, isTransient) {
  val entries = new HashMap[String, List[DictionarySubEntry]]()
  private def staticMetaClass: MetaClass = objectSystem.dictionary2MetaClass
  def metaClass = objectSystem.dictionary2MetaClass
  def addEntry(key: String, subEntries: List[DictionarySubEntry]) {
    entries(key) = subEntries
  }

  def addEntry(key: String, subEntry: DictionarySubEntry) {
    if (!entries.contains(key)) entries(key) = Nil
    if (!entries(key).exists(e => e == subEntry)) {
      entries(key) = subEntry :: entries(key)
    }
  }

  // for dictionaries, we search the static property list, which is
  // in the object's meta class hierarchy
  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("collection prop idx = %d\n", idx)
    val prop = staticMetaClass.callMethodWithIndex(this, idx, argc)
    if (prop != InvalidPropertyId) new Property(propertyId, prop, id)
    else super.getProperty(propertyId, argc)
  }

  def addWord(obj: T3Value, str: T3Value, vocabProp: T3Value): T3Value = {
    val key = objectSystem.toTadsString(str).string
    printf("addWord(%s, %s, %s)\n", obj, key, vocabProp)
    addEntry(key, DictionarySubEntry(obj.value, vocabProp.value))
    T3Nil
  }
}

class Dictionary2MetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "dictionary2"
  val FunctionVector = Array(undef _,   setComparator _, findWord _,
                             addWord _, removeWord _,    isWordDefined _,
                             forEachWord _)

  def undef(obj: T3Object, argc: Int): T3Value = InvalidPropertyId

  def setComparator(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("setComparator")
  }
  def findWord(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("findWord")
  }
  def addWord(obj: T3Object, argc: Int): T3Value = {
    argc must_== 3
    obj.asInstanceOf[Dictionary2].addWord(vmState.stack.pop,
                                          vmState.stack.pop,
                                          vmState.stack.pop)
  }
  def removeWord(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("removeWord")
  }
  def isWordDefined(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isWordDefined")
  }
  def forEachWord(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("forEachWord")
  }

  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    val comparatorObjId     = imageMem.intAt(objDataAddr)
    val loadImageEntryCount = imageMem.shortAt(objDataAddr + 4)
    printf("Dictionary::createFromImage() id = %s, comparator = %d, # entries: %d\n",
           objectId, comparatorObjId, loadImageEntryCount)
    val dict = new Dictionary2(objectId, vmState, isTransient,
                               T3ObjectId(comparatorObjId))
    var ptr = objDataAddr + 6

    for (i <- 0 until loadImageEntryCount) {
      val numKeyBytes = imageMem.byteAt(ptr)
      val keyBytes = new Array[Byte](numKeyBytes)
      ptr += 1
      for (b <- 0 until numKeyBytes) {
        keyBytes(b) = (imageMem.byteAt(ptr + b) ^ 0xbd).asInstanceOf[Byte]
      }
      val key = new String(keyBytes, "UTF-8")
      ptr += numKeyBytes
      val numSubEntries = imageMem.shortAt(ptr)
      //printf("DICT-KEY = '%s' #subentries: %d\n", key, numSubEntries)
      ptr += 2
      var subEntries : List[DictionarySubEntry] = Nil
      for (e <- 0 until numSubEntries) {
        subEntries = DictionarySubEntry(
          associatedObjectId = imageMem.intAt(ptr),
          definingPropertyId  = imageMem.shortAt(ptr + 4)) :: subEntries
        ptr += 6
      }
      dict.addEntry(key, subEntries.reverse)
    }
    dict
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
