/*
 * Created on 2010/04/08
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
package org.zmpp.glk

import java.util.logging._
import java.io.File
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
 * Byte array based file stream.
 */
object GlkFileStream {
  val InitialBufferSize  = 1024
  val BufferIncreaseSize = 1024
}

class GlkFileStream(val rock: Int) extends GlkStream {
  val logger = Logger.getLogger("glk")
  private var _buffer = new Array[Byte](GlkFileStream.InitialBufferSize)
  private def resizeIfNecessary {
    if (position >= _buffer.length) {
      val newbuffer = new Array[Byte](_buffer.length + GlkFileStream.BufferIncreaseSize)
      System.arraycopy(_buffer, 0, newbuffer, 0, _buffer.length)
      _buffer = newbuffer
    }
  }

  def style: Int = throw new UnsupportedOperationException("can not read style from file stream")
  def style_=(s:Int) = throw new UnsupportedOperationException("can not set style in file stream")

  protected var _readCount  = 0
  protected var _writeCount = 0
  protected var _pos        = 0
  protected var _size       = 0

  var id         = 0
  def writeCount = _writeCount
  def readCount  = _readCount
  def size       = _size
  def position   = _pos
  def close {
    logger.info("CLOSING FILE !!!")
    val builder = new StringBuilder
    for (i <- 0 until size) {
      builder.append(_buffer(i).toChar)
    }
    logger.info("Buffer is: [%s]".format(builder.toString))
    // TODO: Do something with te data in this file
  }
  def putChar(c: Char) {
    resizeIfNecessary
    if (c > 255) _buffer(_pos)  = '?'.toByte
    else _buffer(_pos) = (c & 0xff).toByte

    _writeCount += 1
    _pos        += 1
    if (_pos > _size) _size = _pos
  }
  def putCharUni(c: Int) {
    throw new UnsupportedOperationException("put_char_uni not yet supported on file stream")
  }
  
  def seek(newpos: Int, seekmode: Int) {
    seekmode match {
      case SeekModes.Start   => _pos  = newpos
      case SeekModes.Current => _pos += newpos
      case SeekModes.End     => _pos  = size + newpos
      case _                 =>
        throw new IllegalArgumentException("Unknown file seek mode: %d".format(seekmode))
    }
    if (_pos > _size) _size = _pos
  }
  def setHyperlink(linkval: Int) {
    throw new UnsupportedOperationException("setHyperlink not supported on file stream")
  }
}

class GlkBinaryFileOutputStream(file: File, rock: Int) extends GlkFileStream(rock) {
  override def readCount: Int = throw new UnsupportedOperationException("can not read from output stream")
}
class GlkTextFileOutputStream(file: File, rock: Int) extends GlkFileStream(rock) {
  override def readCount: Int = throw new UnsupportedOperationException("can not read from output stream")
}

class FileReference(val id        : Int,
                    val usageType : Int,
                    val filemode  : Int,
                    val file      : File,
                    val rock      : Int) {
  def isBinaryMode = (usageType & 0x100) == 0
  def isTextMode   = (usageType & 0x100) == 0x100
  def fileType     = usageType & FileUsageTypes.TypeMask
  def exists        = file.exists
}

class GlkFileSystem {
  private var _nextId = 1
  private var _fileRefs: List[FileReference] = Nil
  
  private def fileRefWithId(id: Int) = _fileRefs.filter(ref => ref.id == id).head

  def createFileRefByName(usageType: Int, name: String, rock: Int) = {
    val fileref = new FileReference(_nextId, usageType, 0, new File(name), rock)
    _fileRefs = fileref :: _fileRefs
    _nextId += 1
    fileref.id
  }
  def createFileRefByFile(usageType: Int, fmode: Int, file: File, rock: Int) = {
    val fileref = new FileReference(_nextId, usageType, fmode, file, rock)
    _fileRefs = fileref :: _fileRefs
    _nextId += 1
    fileref.id
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
  
  // TODO: This has to work whether Applet or not !!
  def doesFileExist(fileRefId: Int): Boolean = fileRefWithId(fileRefId).exists
  
  def openFile(fileRefId: Int, fmode: Int, rock: Int): GlkStream = {
    val fileref = fileRefWithId(fileRefId)
    val fp: GlkStream = fmode match {
      case FileModes.Write       =>
        if (fileref.isBinaryMode) new GlkBinaryFileOutputStream(fileref.file, rock)
        else new GlkTextFileOutputStream(fileref.file, rock)
      case FileModes.Read        =>
        throw new UnsupportedOperationException("Read file mode not supported yet")
      case FileModes.ReadWrite   =>
        throw new UnsupportedOperationException("ReadWrite file mode not supported yet")
      case FileModes.WriteAppend =>
        throw new UnsupportedOperationException("WriteAppend file mode not supported yet")
      case _               =>
        throw new IllegalArgumentException("Unknown file mode: %d".format(fmode))
    }
    fp
  }
}

