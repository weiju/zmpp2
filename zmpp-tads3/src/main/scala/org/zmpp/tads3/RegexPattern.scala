/*
 * Created on 2010/10/14
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

import java.util.ArrayList
import java.util.regex._
import scala.collection.JavaConversions._
import org.zmpp.base._
import T3Assert._

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
      srcPatternString = objectSystem.toTadsString(srcPattern)
    }
    srcPatternString
  }
  def javaPatternString: String = {
    if (javaPattern == null) {
      val res = new RegexTranslator(patternString.string).translate
      noCase = !res._1
      javaPattern = res._2
    }
    javaPattern
  }

  def compile = {
    if (regexPattern == null ) regexPattern = Pattern.compile(javaPatternString)
    regexPattern
  }

  // Note: I did not get the semantics of matches() at first:
  // The idea is that it should match a subststring of any length starting
  // at index. If the match does not start at index, there is no match
  def matches(str: TadsString, index: Int): T3Value = {
    currentMatcher = compile.matcher(str.string)
    if (currentMatcher.find(index - 1) &&
        currentMatcher.start() == index - 1) {
      T3Integer(currentMatcher.group.length)
    } else T3Nil
  }

  def search(str: TadsString, index: Int): TadsList = {
    currentMatcher = compile.matcher(str.string)
    val foundIndex =
      if (currentMatcher.find(index - 1)) currentMatcher.start + 1 else 0
    if (foundIndex > 0) {
      val groupStr =
        objectSystem.stringMetaClass.createString(currentMatcher.group)
      val resultSeq = List(T3Integer(foundIndex),
                           T3Integer(groupStr.length), groupStr.id)
      objectSystem.listMetaClass.createList(resultSeq)
    } else null
  }

  def group(groupNum: Int): TadsList = {
    if (groupNum <= currentMatcher.groupCount) {
      val groupStr =
        objectSystem.stringMetaClass.createString(currentMatcher.group(groupNum))
      val resultSeq = List(T3Integer(currentMatcher.start(groupNum) + 1),
                           T3Integer(groupStr.length), groupStr.id)
      objectSystem.listMetaClass.createList(resultSeq)
    } else null
  }

  override def toString = {
    "regex-pattern = '%s' (len = %d)".format(patternString, patternString.length)
  }
}

class RegexPatternMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
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

  def createFromTadsString(str: TadsString): RegexPattern = {
    val pattern = new RegexPattern(objectSystem.newObjectId, vmState, false)
    pattern.init(str.id)
    objectSystem.registerObject(pattern)
    pattern
  }

  override def createFromStack(id: T3ObjectId, argc: Int,
                               isTransient: Boolean) = {
    argc must_== 1
    createFromTadsString(objectSystem.toTadsString(vmState.stack.pop))
  }
}
