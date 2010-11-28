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

class TadsString(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {

  var string: String = null

  protected def staticMetaClass = objectSystem.stringMetaClass
  def metaClass = objectSystem.stringMetaClass
  def init(str: String) {
    printf("init string constant: %s\n", str)
    this.string = str
  }
  override def toString = "'%s'".format(string)
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

  override def +(other: T3Object): T3Object = {
    val newStr = new TadsString(objectSystem.newObjectId, vmState, false)
    newStr.init(this.string + other.asInstanceOf[TadsString].string)
    objectSystem.registerObject(newStr)
    newStr
  }
}

class TadsStringConstant(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends TadsString(id, vmState, isTransient) {
}

class StringMetaClass extends AbstractMetaClass {
  def name = "string"

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
}
