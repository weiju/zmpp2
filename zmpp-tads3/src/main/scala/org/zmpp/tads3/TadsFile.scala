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

import T3Assert._

class FileException extends Exception
class FileNotFoundException extends FileException

object TadsFile {
  val FileAccessRead           = 1
  val FileAccessWrite          = 2
  val FileAccessReadWriteKeep  = 3
  val FileAccessReadWreteTrunc = 4

  // TADS 3 system specific file ids that can be used instead of
  // file names
  val LibraryDefaultsFile = 1
}

class FileMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "file"

  val FunctionVector = Array(undef _,           openTextFile _,    openDataFile _,
                             openRawFile _,     getCharacterSet _, setCharacterSet _,
                             closeFile _,       readFile _,        writeFile _,
                             readBytes _,       writeBytes _,      getPos _,
                             setPos _,          setPosEnd _,       openTextResource _,
                             openRawResource _, getFileSize _)

  def undef(obj: T3Object, argc: Int): T3Value = InvalidPropertyId
  def openTextFile(obj: T3Object, argc: Int): T3Value = {
    argc mustBeInRange(2, 3)
    printf("openTextFile(), argc = %d\n", argc)
    val filename = vmState.stack.pop
    val access = vmState.stack.pop
    //val charset = if (argc == 3) vmState.stack.pop else ascii
    printf("filename: %s, access: %s\n", filename, access)
    if (filename.valueType == TypeIds.VmInt) {
      if (filename.value == TadsFile.LibraryDefaultsFile) {
        // the reference implementation looks for /tmp/settings.txt here
        println("library defaults file")
        throw new FileNotFoundException
      } else {
        throw new UnsupportedOperationException("unsupported special file")
      }
    }
    else throw new UnsupportedOperationException("openTextFile")
  }
  def openDataFile(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("openTextFile")
  }
  def openRawFile(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("openRawFile")
  }
  def getCharacterSet(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getCharacterSet")
  }
  def setCharacterSet(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("setCharacterSet")
  }
  def closeFile(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("closeFile")
  }
  def readFile(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("readFile")
  }
  def writeFile(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("writeFile")
  }
  def readBytes(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("readBytes")
  }
  def writeBytes(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("writeBytes")
  }
  def getPos(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getPos")
  }
  def setPos(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("setPos")
  }
  def setPosEnd(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("setPosEnd")
  }
  def openTextResource(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("openTextResource")
  }
  def openRawResource(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("openRawResource")
  }
  def getFileSize(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getFileSize")
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
