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
import java.util.regex._
import scala.collection.JavaConversions._
import org.zmpp.base._

// RegexPatterns are stored in the image as pointers to string constants
// For efficiency, the underlying pattern strings are retrieved when a
// pattern is actually used.
// When that happens, the pattern string is translated to a Java regex
// which is then compiled using the standard library, since it does not
// make any sense to implement my own
class RegexPattern(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {
  private var srcPattern: T3Value          = T3Nil
  private var srcPatternString: TadsString = null
  private var javaPattern: String          = null
  private var noCase: Boolean = false
  private var regexPattern: Pattern = null
  private var currentMatcher: Matcher = null

  def metaClass = objectSystem.regexPatternMetaClass
  def init(value: T3Value) {
    srcPattern = value
  }
  def ignoreCase = noCase

  def patternString: TadsString = {
    if (srcPatternString == null) {
      srcPatternString =
        objectSystem.stringConstantWithOffset(
          srcPattern.asInstanceOf[T3SString]).asInstanceOf[TadsString]
    }
    srcPatternString
  }
  def javaPatternString: String = {
    if (javaPattern == null) {
      noCase = patternString.string.indexOf("<nocase>") >= 0
      // TODO: convert to a java pattern string
      // TODO: '%%' sequences !!
      // TODO: angled expression replacements should be case insensitive !!
      javaPattern = patternString.string.replaceAll("<nocase>", "")
      javaPattern = javaPattern.replaceAll("<langle>", "<")
      javaPattern = javaPattern.replaceAll("<rangle>", ">")
      javaPattern = javaPattern.replaceAll("%", "\\\\")
    }
    javaPattern
  }

  def compile = {
    if (regexPattern == null ) regexPattern = Pattern.compile(javaPatternString)
    regexPattern
  }

  def search(str: TadsString, index: Int): Int = {
    currentMatcher = compile.matcher(str.string)
    if (currentMatcher.find(index - 1)) currentMatcher.start + 1 else 0
  }

  def group(groupNum: Int): TadsList = {
    if (groupNum <= currentMatcher.groupCount) {
      val resultList = objectSystem.listMetaClass.createList(objectSystem.newObjectId)
      objectSystem.registerObject(resultList)
      val groupStr =
        objectSystem.stringMetaClass.createString(objectSystem.newObjectId,
                                                  currentMatcher.group(groupNum))
      objectSystem.registerObject(groupStr)
      val resultSeq = List(new T3Integer(currentMatcher.start(groupNum) + 1),
                           new T3Integer(groupStr.length), groupStr.id)
      resultList.initWith(resultSeq)
      resultList
    } else null
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
