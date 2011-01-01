/*
 * Created on 2010/04/08
 * Copyright (c) 2010-2011, Wei-ju Wu.
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
package org.zmpp.glk

import java.util.logging._
import java.io.File
import java.io.RandomAccessFile
import scala.collection.mutable._

/*
 * File management for Glk.
 * In this implementation of the Glk, files are memory objects, which are
 * then written to the native file system if necessary.
 * Alternatively, there is a memory file system, which allows to write files
 * as temporary memory objects.
 */
object FileUsageTypes {
  val Data        = 0x00
  val SavedGame   = 0x01
  val Transcript  = 0x02
  val InputRecord = 0x03
  val TypeMask    = 0x0f
  val TextMode    = 0x100
  val BinaryMode  = 0x000
}

object FileModes {
  val Write       = 0x01
  val Read        = 0x02
  val ReadWrite   = 0x03
  val WriteAppend = 0x05
}

object SeekModes {
  val Start       = 0
  val Current     = 1
  val End         = 2
}

/**
 * File streams are based on RandomAccessFile, this is probably the only
 * way to allow for all the operations that streams require.
 */
class GlkFileStream(fileRef: FileReference,
                    val fmode: Int,
                    val rock: Int,
                    val isUnicode: Boolean) extends GlkStream {
  val logger = Logger.getLogger("glk")
  if (fileRef.fmode != 0 && fmode != fileRef.fmode) {
    logger.warning("FileStream FMODE != FileRef FMODE !!!")
  }
  logger.info("Opening file '%s' with usage: %d and fmode: %d".format(
              fileRef.file.getName, fileRef.usage, fmode))
  val realFile = new RandomAccessFile(fileRef.file, fileOpenMode)
  if (fmode == FileModes.WriteAppend) {
    realFile.seek(realFile.length)
  } else if (fmode == FileModes.Write) {
    // overwrite everything
    realFile.setLength(0)
  }

  private def fileOpenMode = {
    if (fileRef.isReadOnly) "r"
    else "rw"
  }
  def style: Int =
    throw new UnsupportedOperationException("can not read style from file stream")
  def style_=(s:Int) =
    throw new UnsupportedOperationException("can not set style in file stream")

  protected var _readCount  = 0
  protected var _writeCount = 0

  var id         = 0
  def size       = realFile.length
  def position   = realFile.getFilePointer.asInstanceOf[Int]
  def close = realFile.close
  def writeCount = _writeCount
  def putChar(c: Char) {
    realFile.writeByte(c & 0xff)
    _writeCount += 1
  }
  def putCharUni(c: Int) {
    realFile.writeChar(c)
    _writeCount += 1
  }
  
  def seek(newpos: Int, seekmode: Int) {
    seekmode match {
      case SeekModes.Start   => realFile.seek(newpos)
      case SeekModes.Current => realFile.seek(position + newpos)
      case SeekModes.End     => realFile.seek(size + newpos)
      case _                 =>
        throw new IllegalArgumentException("Unknown file seek mode: %d".format(
          seekmode))
    }
  }
  def setHyperlink(linkval: Int) {
    throw new UnsupportedOperationException("setHyperlink not supported on file stream")
  }
  def readCount = _readCount
  def getChar : Int = {
    if (position >= realFile.length) return -1
    _readCount += 1
    realFile.readByte.asInstanceOf[Int]
  }
  def getCharUni : Int = {
    if (position >= realFile.length) return -1
    _readCount += 1
    realFile.readChar.asInstanceOf[Int]
  }
}

class FileReference(val id        : Int,
                    val usage     : Int,
                    val fmode     : Int,
                    val file      : File,
                    val rock      : Int) {
  def isBinaryMode = (usage & 0x100) == 0
  def isTextMode   = (usage & 0x100) == 0x100
  def fileType     = usage & FileUsageTypes.TypeMask
  def exists       = file.exists
  def isReadOnly   = fmode == FileModes.Read
  def isWriteOnly  = isAppend || fmode == FileModes.Write
  def isReadWrite  = fmode == FileModes.ReadWrite
  def isAppend     = fmode == FileModes.WriteAppend
}

class GlkFileSystem {
  private var _nextId = 1
  private var _fileRefs: List[FileReference] = Nil
  
  private def fileRefWithId(id: Int) = _fileRefs.filter(ref => ref.id == id).head
  private def addFileRef(usage: Int, fmode: Int, file: File, rock: Int): Int = {
    val fileref = new FileReference(_nextId, usage, fmode, file, rock)
    _fileRefs = fileref :: _fileRefs
    _nextId += 1
    fileref.id
  }
  
  def createFileRefByName(usage: Int, name: String, rock: Int) = {
    addFileRef(usage, 0, new File(name), rock)
  }
  def createFileRefByFile(usage: Int, fmode: Int, file: File, rock: Int) = {
    addFileRef(usage, fmode, file, rock)
  }
  def createFromFileRef(usage: Int, fileRefId: Int, rock: Int) = {
    val refFileRef = fileRefWithId(fileRefId)
    addFileRef(usage, refFileRef.fmode, refFileRef.file, rock)
  }
  def createTemp(usage: Int, rock: Int) = {
    addFileRef(usage, 0, File.createTempFile("zmpp-glk", "tmp"), rock)
  }
  def deleteFile(fileRefId: Int) {
    val fileRef = fileRefWithId(fileRefId)
    if (fileRef != null && fileRef.file.exists) {
      fileRef.file.delete
    }
  }
  def destroy(fileRefId: Int) {
    _fileRefs = _fileRefs.filterNot(fileRef => fileRef.id == fileRefId)
  }
  def getRockForFileRef(fileRefId: Int) = fileRefWithId(fileRefId).rock
  
  def iterate(id: Int): FileReference = {
    if (id == 0) if (_fileRefs.isEmpty) null else _fileRefs.head
    else {
      val remain = _fileRefs.dropWhile(ref => ref.id != id).tail
      if (remain.isEmpty) null
      else remain.head
    }
  }
  
  def doesFileExist(fileRefId: Int): Boolean = fileRefWithId(fileRefId).exists
  
  def openFile(fileRefId: Int, fmode: Int, rock: Int): GlkStream = {
    new GlkFileStream(fileRefWithId(fileRefId), fmode, rock, false)
  }
  def openFileUni(fileRefId: Int, fmode: Int, rock: Int): GlkStream = {
    throw new UnsupportedOperationException("Unicode files not supported yet")
    // new GlkFileStream(fileRefWithId(fileRefId), fmode, rock, true)
  }
}

