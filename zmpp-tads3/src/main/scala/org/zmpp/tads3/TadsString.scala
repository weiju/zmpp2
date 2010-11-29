/*
 * Created on 2010/11/27
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
import TypeIds._
import T3Assert._

// Strings are implemented with Java strings, but their index is based on
// 1 instead of 0
object TadsString {
  val ReplaceAll = 1
}
class TadsString(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {

  var string: String = null

  protected def staticMetaClass = objectSystem.stringMetaClass
  def metaClass = objectSystem.stringMetaClass
  def init(str: String) = this.string = str
  def length = string.length
  override def toString = string
  override def t3vmEquals(other: T3Value): Boolean = {
    if (other.valueType == VmSString) {
      val otherString =
        objectSystem.stringConstantWithOffset(other.asInstanceOf[T3SString])
      printf("string.t3vmEquals() [%s], other = %s\n", this, otherString)
      this.string.equals(otherString.asInstanceOf[TadsString].string)
    } else {
      throw new UnsupportedOperationException("unsupported T3value type")
    }
  }
  private def createStringFrom(str: String): TadsString = {
    val newStr = new TadsString(objectSystem.newObjectId, vmState, false)
    newStr.init(str)
    objectSystem.registerObject(newStr)
    newStr
  }

  override def +(other: T3Object): T3Object = {
    createStringFrom(this.string + other.asInstanceOf[TadsString].string)
  }

  // for strings, we search the static property list
  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("string prop idx = %d\n", idx)
    if (idx >= 0) {
      new Property(propertyId,
                   staticMetaClass.callMethodWithIndex(this, idx, argc),
                   id)
    } else super.getProperty(propertyId, argc)
  }

  def find(str: TadsString, index: Int): Int = {
    string.indexOf(str.string, index - 1) + 1
  }

  def findReplace(origStr: TadsString, newStr: TadsString, replaceAll: Boolean,
                  index: Int): TadsString = {
    printf("findReplace(%s, %s, %b, %d)\n", origStr, newStr, replaceAll, index)
    val result = if (index > 1) {
      if (replaceAll) {
        string.substring(0, index - 1) +
          string.substring(index - 1).replaceAll(origStr.string, newStr.string)
      } else {
        string.substring(0, index - 1) +
          string.substring(index - 1).replaceFirst(origStr.string, newStr.string)
      }
    } else {
      if (replaceAll) string.replaceAll(origStr.string, newStr.string)
      else string.replaceFirst(origStr.string, newStr.string)
    }
    printf("findReplace(), result string is: %s\n", result)
    createStringFrom(result)
  }
}

class TadsStringConstant(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsString(id, vmState, isTransient) {
}

class StringMetaClass extends AbstractMetaClass {
  def name = "string"

  val FunctionVector = Array(undef _,        length _,          substr _,
                             toUpper _,      toLower _,         find _,
                             toUnicode _,    htmlify _,         startsWith _,
                             endsWith _,     mapToByteArray _,  findReplace _)

  def undef(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("undefined")
  }
  def length(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("length")
  }
  def substr(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("substr")
  }
  def toUpper(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("toUpper")
  }
  def toLower(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("toLower")
  }
  def find(obj: T3Object, argc: Int): T3Value = {
    argCountMustBe(argc, 1, 2)
    val str = objectSystem.toT3Object(vmState.stack.pop)
    val index = if (argc > 1) vmState.stack.pop.value else 1
    val foundAt = obj.asInstanceOf[TadsString].find(
      str.asInstanceOf[TadsString], index)
    printf("find(%s, %d) = %d\n", str, index, foundAt)
    if (foundAt == 0) T3Nil else new T3Integer(foundAt)
  }
  def toUnicode(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("toUnicode")
  }
  def htmlify(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("htmlify")
  }
  def startsWith(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("startsWith")
  }
  def endsWith(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("endsWith")
  }
  def mapToByteArray(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("toByteArray")
  }
  def findReplace(obj: T3Object, argc: Int): T3Value = {
    import TadsString._
    argCountMustBe(argc, 3, 4)
    val origStr = objectSystem.toT3Object(vmState.stack.pop)
    val newStr  = objectSystem.toT3Object(vmState.stack.pop)
    val replaceAll = (vmState.stack.pop.value & ReplaceAll) == ReplaceAll
    val index   = if (argc == 4) vmState.stack.pop.value else 1
    obj.asInstanceOf[TadsString].findReplace(origStr.asInstanceOf[TadsString],
                                             newStr.asInstanceOf[TadsString],
                                             replaceAll, index).id
  }

  def createStringConstant(id: T3ObjectId, offset: T3SString): TadsString = {
    val len = vmState.image.constantDataShortAt(offset.value)
    val dataStart = offset.value + 2 
    val builder = new StringBuilder
    for (i <- 0 until len) {
      val c = vmState.image.constantDataByteAt(dataStart + i).asInstanceOf[Char]
      if (c > 127) throw new UnsupportedOperationException("no unicode yet")
      builder.append(c)
    }
    val stringConst = new TadsStringConstant(id, vmState, false)
    stringConst.init(builder.toString)
    stringConst
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
