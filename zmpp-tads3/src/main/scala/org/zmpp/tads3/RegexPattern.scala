/*
 * Created on 2010/10/14
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

import java.util.ArrayList
import scala.collection.JavaConversions._
import org.zmpp.base._

// regex patterns are stored in the image as pointers to string constants
class RegexPattern(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {
  var srcPattern: T3Value = T3Nil

  def metaClass = objectSystem.regexPatternMetaClass
  def init(value: T3Value) {
    srcPattern = value
  }

  def compile {
    val str = objectSystem.stringConstantWithOffset(srcPattern.asInstanceOf[T3SString])
  }

  override def toString = {
    val str = objectSystem.stringConstantWithOffset(srcPattern.asInstanceOf[T3SString])
    "regex-pattern [%s]".format(str)
  }
}

class RegexPatternMetaClass extends AbstractMetaClass {
  def name = "regex-pattern"

  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    val regexPat = new RegexPattern(objectId, vmState, isTransient)
    val sourceVal = T3Value.readDataHolder(imageMem, objDataAddr)
    if (sourceVal.valueType == TypeIds.VmSString) {
      regexPat.init(sourceVal)
      regexPat
    } else {
      throw new UnsupportedOperationException("unsupported regex data type")
    }
  }
}
