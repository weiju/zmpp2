/*
 * Created on 2010/05/08
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

// we probably will replace the map for static objects with a list in the future
// for now, the tree map helps to synchronize better with the reference
// implementation
import scala.collection.JavaConversions._
import java.util.TreeMap

import scala.collection.mutable.HashMap
import org.zmpp.base._

// This file contains the data that is read from a TADS3 image file.
// As understanding grows, I add information. Note that TADS3 images are
// stored in little endian format (*yuck*).

object ImageFile {
  // The TADS3 signature ("T3-image\015\12\032")
  val Signature = Array[Byte](0x54, 0x33, 0x2d, 0x69, 0x6d, 0x61, 0x67, 0x65,
                              0x0d, 0x0a, 0x1a)
}

// Block header, structure:
// Bytes 0-3: block type ID
// Bytes 4-7: block size in bytes (UINT4)
// Bytes 8-9: Flags (UINT2), only flag 0 is currently used (the mandatory flag)
// Block type ID can be one of
// - EOF:  last data block, marks the end of the file
// - ENTP: Entrypoint, marks the beginning of executable code
// - OBJS: Static Object
// - CPDF: Constant Pool Definition
// - CPPG: Constant Pool Page
// - MRES: Multimedia Resource
// - MREL: Multimedia Rsource Link
// - MCLD: Metaclass Dependency List
// - FNSD: Function Set Dependency List
// - SYMD: Symbolic Names
// - SRCF: Source File Descriptor
// - GSYM: Global Symbol Table
// - MHLS: Method Header List
// - MACR: Macro Symbol Table
// - SINI: Static Initializer List
object BlockHeader {
  val Size = 10
}
class BlockHeader(val address: Int, val typeId: String, val dataSize: Int,
                  val flags: Int) {
  def dataAddress = address + BlockHeader.Size
  override def toString = {
    "DataBlock[%s, %d, %d]".format(typeId, dataSize, flags)
  }
}

// References to constant pools in the image file. Constant pools are
// implemented as collections of pool pages, which are in turn simple
// pointers to the address within the image data.
object PoolTypes {
  val ByteCode     = 1
  val ConstantData = 2
}
class ConstantPoolPage(val dataAddress: Int, size: Int)

class ConstantPool(val id: Int, val numPages: Int, val pageSize: Int) {
  val _pages = new Array[ConstantPoolPage](numPages)
  
  def addPage(pageIndex: Int, page: ConstantPoolPage) = _pages(pageIndex) = page
  def addressForOffset(offset: Int) = {
    val pageIndex = offset / pageSize
    val offsetInPage = offset % pageSize
    _pages(pageIndex).dataAddress + offsetInPage
  }
  override def toString = {
    "ConstantPool[%d, %d, %d]".format(id, numPages, pageSize)
  }
}

// Function set dependency. The image file defines the index that is used
// to access a function set by listing the order of function set dependencies
// in the function set dependency block
class FunctionSetDependency(nameString: String) {
  val name = nameString.split("/")(0)
  val version = if (nameString.split("/").length == 2) nameString.split("/")(1)
                else "000000"
}

// Symbols are publicly known values in the image file that are
// referenced by a textual name. The image file provides a mapping
// from the name to the way the symbol can be accessed by the VM
class SymbolicName(val name: String, val valueType: Int, val value: Int) {
  val t3Value = T3Value.create(valueType, value)
  override def toString = "SymbolicName['%s' = %s]".format(name, t3Value)
}

class MethodHeader(val paramCount: Int, val localCount: Int,
                   val maxStackSlots: Int,
                   val exceptionTableOffset: Int, val debugRecordOffset: Int)

/**
 * Quickly construct a TADS3 image from a memory object.
 * Quite a bit of data is loaded on demand.
 */
class TadsImage(val memory: Memory) {
  private var _timestamp: String = null
  private var _blocks: List[BlockHeader] = Nil
  private val _constantPools = new Array[ConstantPool](3)
  private var _entp: BlockHeader = null
  private var _functionSetDependencies: Array[FunctionSetDependency] = null
  private val _symbolicNames = new HashMap[String, SymbolicName]
  private var _maxObjectId = 0

  def maxObjectId             = _maxObjectId
  def symbolicNames           = _symbolicNames
  def functionSetDependencies = _functionSetDependencies

  // read data from blocks to build an index
  // This function needs to be called in order to initialize the system
  def readData(vmState: TadsVMState) {
    var blockAddress = 69
    var blockHeader = readBlockHeader(blockAddress)
    while (blockHeader.typeId != "EOF ") {

      blockHeader.typeId match {
        case "CPDF" => readPoolDef(blockHeader)
        case "CPPG" => addConstantPoolPage(blockHeader)
        case "ENTP" => _entp = blockHeader
        case "FNSD" => readFunctionSetDependencies(blockHeader)
        case "MCLD" => readMetaclassDependencies(blockHeader, vmState.objectSystem)
        case "OBJS" => readStaticObjectBlock(blockHeader, vmState.objectSystem)
        case "SYMD" => readSymbolicNameBlock(blockHeader)
        case _ =>
          printf("UNHANDLED BLOCK: %s\n", blockHeader.toString)
      }
      blockAddress += BlockHeader.Size + blockHeader.dataSize
      blockHeader = readBlockHeader(blockAddress)
    }
    /*
     printf("# blocks read: %d\n", _blocks.length)
     printf("start address: $%02x\n", startAddress)
     printf("method header size: %d\n", methodHeaderSize)
     printf("ex table entry size: %d\n", exTableEntrySize)
     printf("# static objects: %d\n", _staticObjects.size)
     printf("MAX object id: %d\n", _maxObjectId)*/
  }

  def startEntryPoint  = memory.intAt(_entp.dataAddress)
  def methodHeaderSize = memory.shortAt(_entp.dataAddress + 4)
  def exTableEntrySize = memory.shortAt(_entp.dataAddress + 6)
  def startAddress     = startEntryPoint + methodHeaderSize
  
  def isValid: Boolean = {
    for (i <- 0 until ImageFile.Signature.length) {
      if (memory.byteAt(i) != ImageFile.Signature(i)) return false
    }
    true
  }
  def version = (memory.byteAt(12) << 8) | memory.byteAt(11).toInt
  def timestamp = {
    if (_timestamp == null) {
      val buffer = new StringBuilder
      for (i <- 45 to 68) buffer.append(memory.byteAt(i).asInstanceOf[Char])
      _timestamp = buffer.toString
    }
    _timestamp
  }
  
  def codeByteAt(offset: Int) = {
    memory.byteAt(_constantPools(PoolTypes.ByteCode).addressForOffset(offset))
  }
  def codeShortAt(offset: Int) = {
    memory.shortAt(_constantPools(PoolTypes.ByteCode).addressForOffset(offset))
  }
  def codeIntAt(offset: Int) = {
    memory.intAt(_constantPools(PoolTypes.ByteCode).addressForOffset(offset))
  }
  def constantDataByteAt(offset: Int) = {
    memory.byteAt(_constantPools(PoolTypes.ConstantData).addressForOffset(offset))
  }
  def constantDataShortAt(offset: Int) = {
    memory.shortAt(_constantPools(PoolTypes.ConstantData).addressForOffset(offset))
  }
  def constantDataIntAt(offset: Int) = {
    memory.intAt(_constantPools(PoolTypes.ConstantData).addressForOffset(offset))
  }
  
  def methodHeaderAt(offset: Int) = {
    val addr = _constantPools(PoolTypes.ByteCode).addressForOffset(offset)
    new MethodHeader(memory.byteAt(addr), memory.shortAt(addr + 2),
                     memory.shortAt(addr + 4), memory.shortAt(addr + 6),
                     memory.shortAt(addr + 8))
  }

  // ********************************************************************
  // ****** Private methods
  // ******************************
  private def printPage(pageAddr: Int, size: Int) {
    for (i <- 0 until size) {
      printf("0x%02x ", memory.byteAt(pageAddr + i))
    }
    println
  }
  private def addConstantPoolPage(blockHeader: BlockHeader) {
    val poolId      = memory.shortAt(blockHeader.dataAddress)
    val pageIndex   = memory.intAt(blockHeader.dataAddress + 2)
    val xor         = memory.byteAt(blockHeader.dataAddress + 6)
    val dataAddress = blockHeader.dataAddress + 7
    val dataSize    = blockHeader.dataSize - 7
    _constantPools(poolId).addPage(pageIndex,
      new ConstantPoolPage(dataAddress, dataSize))

    if (xor != 0) {
      for (i <- 0 until dataSize) {
        memory.setByteAt(dataAddress + i,
                         memory.byteAt(dataAddress + i) ^ xor)
      }
    }
  }

  private def readPoolDef(blockHeader: BlockHeader) {
    val addr = blockHeader.dataAddress
    val poolId = memory.shortAt(addr)
    _constantPools(poolId) = new ConstantPool(poolId, memory.intAt(addr + 2),
                                              memory.intAt(addr + 6))
  }
  
  private def readBlockHeader(addr: Int): BlockHeader = {
    val buffer = new StringBuilder
    buffer.append(memory.byteAt(addr).asInstanceOf[Char])
    buffer.append(memory.byteAt(addr + 1).asInstanceOf[Char])
    buffer.append(memory.byteAt(addr + 2).asInstanceOf[Char])
    buffer.append(memory.byteAt(addr + 3).asInstanceOf[Char])
    new BlockHeader(addr, buffer.toString, memory.intAt(addr + 4),
                    memory.shortAt(addr + 8).asInstanceOf[Int])
  }
  
  private def readMetaclassDependencies(blockHeader: BlockHeader,
                                        objectSystem: ObjectSystem) {
    val numEntries = memory.shortAt(blockHeader.dataAddress)
    var addr = blockHeader.dataAddress + 2
    for (i <- 0 until numEntries) {
      val entrySize = memory.shortAt(addr)
      val numEntryNameBytes = memory.byteAt(addr + 2)
      val namebuffer = new StringBuilder
      for (j <- 0 until numEntryNameBytes) {
        namebuffer.append(memory.byteAt(addr + 3 + j).asInstanceOf[Char])
      }
      val numPropertyIds = memory.shortAt(addr + 3 + numEntryNameBytes)
      objectSystem.addMetaClassDependency(i, namebuffer.toString)
      val propbase = addr + 3 + numEntryNameBytes + 2

      // this does not seem to be documented: there are not numPropertyIds
      // mappings here, but (numPropertyIds + 1) !!!
      // One possible explanation might be that function index 0 is
      // always mapped to the undef function
      for (j <- 0 to numPropertyIds) {
        objectSystem.addMetaClassPropertyId(i, j, memory.shortAt(propbase + j * 2))
      }
      addr += entrySize
    }
  }
  
  private def readFunctionSetDependencies(blockHeader: BlockHeader) {
    val numEntries = memory.shortAt(blockHeader.dataAddress)
    _functionSetDependencies = new Array[FunctionSetDependency](numEntries)
    var addr = blockHeader.dataAddress + 2
    for (i <- 0 until numEntries) {
      val numBytes = memory.byteAt(addr)
      val buffer = new StringBuilder
      for (j <- 0 until numBytes) {
        buffer.append(memory.byteAt(addr + 1 + j).asInstanceOf[Char])
      }
      _functionSetDependencies(i) = new FunctionSetDependency(buffer.toString)
      addr += numBytes + 1
    }    
  }

  private def readStaticObjectBlock(blockHeader: BlockHeader,
                                    objectSystem: ObjectSystem) {
    val dataAddress = blockHeader.dataAddress
    val numObjects     = memory.shortAt(dataAddress)
    val metaClassIndex = memory.shortAt(dataAddress + 2)
    val flags          = memory.shortAt(dataAddress + 4)
    // for large objects, the size field is size UINT4, otherwise
    // UINT2
    val isLarge     = (flags & 0x01) == 0x01
    val isTransient = (flags & 0x02) == 0x02
    var objAddr = dataAddress + 6
    //printf("# objects = %d meta class: %d large: %b transient: %b\n",
    //  numObjects, metaClassIndex, isLarge, isTransient)
    for (i <- 0 until numObjects) {
      val objId = memory.intAt(objAddr)
      if (objId > _maxObjectId) _maxObjectId = objId
      val numBytes = if (isLarge) memory.intAt(objAddr + 4)
                     else memory.shortAt(objAddr + 4)
      //printf("OBJ ID: %d #BYTES: %d\n", objId, numBytes)
      objAddr += (if (isLarge) 8 else 6)
      objectSystem.addStaticObject(objId, metaClassIndex, objAddr, numBytes,
                                    isTransient)
      objAddr += numBytes
    }
  }
  
  private def readSymbolicNameBlock(blockHeader: BlockHeader) {
    var current = blockHeader.dataAddress
    val numEntries = memory.shortAt(current)
    current += 2
    printf("# SYMBOLIC NAMES: %d\n", numEntries)
    for (i <- 0 until numEntries) {
      val valueType = memory.byteAt(current)
      val value = DataHolder.valueForType(valueType, memory.intAt(current + 1))
      val numChars = memory.byteAt(current + 5)
      // construct name
      val builder = new StringBuilder
      for (j <- 0 until numChars) {
        builder.append(memory.byteAt(current + 6 + j).asInstanceOf[Char])
      }
      val name = builder.toString
      _symbolicNames(name) = new SymbolicName(name, valueType, value)
      printf("Adding symbol: %s\n", _symbolicNames(name))
      current += 6 + numChars
    }
  }
}


