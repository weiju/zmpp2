package org.zmpp.tads3

import scala.collection.mutable.HashMap
import org.zmpp.base._

object Tads3Header {
  val Id = Array[Byte](0x54, 0x33, 0x2d, 0x69, 0x6d, 0x61, 0x67, 0x65, 0x0d, 0x0a, 0x1a)
}

object BlockHeader {
  val Size = 10
}
class BlockHeader(val address: Int, val typeId: String, val dataSize: Int, val flags: Int) {
  def dataAddress = address + BlockHeader.Size
  override def toString = {
    "DataBlock[%s, %d, %d]".format(typeId, dataSize, flags)
  }
}

object PoolTypes {
  val ByteCode     = 1
  val ConstantData = 2
}
class ConstantPoolPage(val dataAddress: Int, size: Int, xorMask: Byte) {
  var _dataBytes: Array[Byte] = null
}

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

class MetaClass(val name: String, val numProperties: Int) {
  val propertyIds = new Array[Int](numProperties)
}

class StaticObject(val id: Int, val metaClassIndex: Int, val dataAddress: Int,
                   val dataSize: Int)

class MethodHeader(val paramCount: Int, val localCount: Int, val maxStackSlots: Int,
                   val exceptionTableOffset: Int, val debugRecordOffset: Int)

/**
 * Quickly construct a TADS3 image from a memory object.
 * Quite a bit of data is loaded on demand.
 */
class Tads3Image(_image: Memory) {
  private var _timestamp: String = null
  private var _blocks: List[BlockHeader] = Nil
  private val _constantPools = new Array[ConstantPool](3)
  private var _entp: BlockHeader = null
  private var _metaClasses: Array[MetaClass] = null
  private var _functionSets: Array[String] = null
  private val _objects = new HashMap[Int, StaticObject]

  // read data from blocks to build an index
  var blockAddress = 69
  var blockHeader = readBlockHeader(blockAddress)
  while (blockHeader.typeId != "EOF ") {

    blockHeader.typeId match {
      case "CPDF" => readPoolDef(blockHeader)
      case "CPPG" => addConstantPoolPage(blockHeader)
      case "ENTP" => _entp = blockHeader
      case "FNSD" => readFunctionSetDependencies(blockHeader)
      case "MCLD" => readMetaclassDependencies(blockHeader)
      case "OBJS" => readStaticObjectBlock(blockHeader)
      case _ =>
        printf("UNHANDLED BLOCK: %s\n", blockHeader.toString)
    }
    blockAddress += BlockHeader.Size + blockHeader.dataSize
    blockHeader = readBlockHeader(blockAddress)
  }
  printf("# blocks read: %d\n", _blocks.length)
  printf("start address: $%02x\n", startAddress)
  printf("method header size: %d\n", methodHeaderSize)
  printf("ex table entry size: %d\n", exTableEntrySize)
  printf("# objects: %d\n", _objects.size)
  
  def startEntryPoint  = _image.intAt(_entp.dataAddress)
  def methodHeaderSize = _image.shortAt(_entp.dataAddress + 4)
  def exTableEntrySize = _image.shortAt(_entp.dataAddress + 6)
  def startAddress     = startEntryPoint + methodHeaderSize
  
  def isValid: Boolean = {
    for (i <- 0 until Tads3Header.Id.length) {
      if (_image.byteAt(i) != Tads3Header.Id(i)) return false
    }
    true
  }
  def version = (_image.byteAt(12) << 8) | _image.byteAt(11).toInt
  def timestamp = {
    if (_timestamp == null) {
      val buffer = new StringBuilder
      for (i <- 45 to 68) buffer.append(_image.byteAt(i).asInstanceOf[Char])
      _timestamp = buffer.toString
    }
    _timestamp
  }
  
  def codeByteAt(offset: Int) = {
    _image.byteAt(_constantPools(PoolTypes.ByteCode).addressForOffset(offset))
  }
  def codeShortAt(offset: Int) = {
    _image.shortAt(_constantPools(PoolTypes.ByteCode).addressForOffset(offset))
  }
  def codeIntAt(offset: Int) = {
    _image.intAt(_constantPools(PoolTypes.ByteCode).addressForOffset(offset))
  }
  
  def methodHeaderAt(offset: Int) = {
    val addr = _constantPools(PoolTypes.ByteCode).addressForOffset(offset)
    new MethodHeader(_image.byteAt(addr), _image.shortAt(addr + 2),
                     _image.shortAt(addr + 4), _image.shortAt(addr + 6),
                     _image.shortAt(addr + 8))
  }

  // ********************************************************************
  // ****** Private methods
  // ******************************
  private def printPage(pageAddr: Int, size: Int) {
    for (i <- 0 until size) {
      printf("0x%02x ", _image.byteAt(pageAddr + i))
    }
    println
  }
  private def addConstantPoolPage(blockHeader: BlockHeader) {
    val poolId    = _image.shortAt(blockHeader.dataAddress)
    val pageIndex = _image.intAt(blockHeader.dataAddress + 2)
    val xor       = _image.byteAt(blockHeader.dataAddress + 6)
    /*
    if (poolId == 1 && pageIndex == 0) {
      printf("first code page found, addr = $%02x size = %d\n",
             blockHeader.dataAddress + 7, blockHeader.dataSize - 7)
      val bcaddr = blockHeader.dataAddress + 7
      printPage(bcaddr, blockHeader.dataSize - 7)
    }*/
    _constantPools(poolId).addPage(pageIndex,
      new ConstantPoolPage(blockHeader.dataAddress + 7,
                           blockHeader.dataSize - 7, xor.asInstanceOf[Byte]))
  }

  private def readPoolDef(blockHeader: BlockHeader) {
    val addr = blockHeader.dataAddress
    val poolId = _image.shortAt(addr)
    _constantPools(poolId) = new ConstantPool(poolId, _image.intAt(addr + 2), _image.intAt(addr + 6))
  }
  
  private def readBlockHeader(addr: Int): BlockHeader = {
    val buffer = new StringBuilder
    buffer.append(_image.byteAt(addr).asInstanceOf[Char])
    buffer.append(_image.byteAt(addr + 1).asInstanceOf[Char])
    buffer.append(_image.byteAt(addr + 2).asInstanceOf[Char])
    buffer.append(_image.byteAt(addr + 3).asInstanceOf[Char])
    new BlockHeader(addr, buffer.toString, _image.intAt(addr + 4),
                    _image.shortAt(addr + 8).asInstanceOf[Int])
  }
  
  private def readMetaclassDependencies(blockHeader: BlockHeader) {
    val numEntries = _image.shortAt(blockHeader.dataAddress)
    _metaClasses = new Array[MetaClass](numEntries)
    printf("# meta classes found: %d\n", numEntries)
    var addr = blockHeader.dataAddress + 2
    for (i <- 0 until numEntries) {
      val entrySize = _image.shortAt(addr)
      val numEntryNameBytes = _image.byteAt(addr + 2)
      val namebuffer = new StringBuilder
      for (j <- 0 until numEntryNameBytes) {
        namebuffer.append(_image.byteAt(addr + 3 + j).asInstanceOf[Char])
      }
      val numPropertyIds = _image.shortAt(addr + 3 + numEntryNameBytes)
      _metaClasses(i) = new MetaClass(namebuffer.toString, numPropertyIds)
      printf("Metaclass %d: %s\n", i, _metaClasses(i).name)
      val propbase = addr + 3 + numEntryNameBytes + 2
      for (j <- 0 until numPropertyIds) {
        _metaClasses(i).propertyIds(j) = _image.shortAt(propbase + j * 2)
      }
      addr += entrySize
    }
  }
  
  private def readFunctionSetDependencies(blockHeader: BlockHeader) {
    val numEntries = _image.shortAt(blockHeader.dataAddress)
    _functionSets = new Array[String](numEntries)
    var addr = blockHeader.dataAddress + 2
    for (i <- 0 until numEntries) {
      val numBytes = _image.byteAt(addr)
      val buffer = new StringBuilder
      for (j <- 0 until numBytes) {
        buffer.append(_image.byteAt(addr + 1 + j).asInstanceOf[Char])
      }
      _functionSets(i) = buffer.toString
      addr += numBytes + 1
    }    
  }

  private def readStaticObjectBlock(blockHeader: BlockHeader) {
    val dataAddress = blockHeader.dataAddress
    val numObjects     = _image.shortAt(dataAddress)
    val metaClassIndex = _image.shortAt(dataAddress + 2)
    val flags          = _image.shortAt(dataAddress + 4)
    val isLarge     = (flags & 0x01) == 0x01
    val isTransient = (flags & 0x02) == 0x02
    var objAddr = dataAddress + 6
    //printf("# objects = %d meta class: %d large: %b transient: %b\n",
    //  numObjects, metaClassIndex, isLarge, isTransient)
    for (i <- 0 until numObjects) {
      val objId = _image.intAt(objAddr)
      val numBytes = if (isLarge) _image.intAt(objAddr + 4)
                     else _image.shortAt(objAddr + 4)
      //printf("OBJ ID: %d #BYTES: %d\n", objId, numBytes)
      objAddr += (if (isLarge) 8 else 6)
      val obj = new StaticObject(objId, metaClassIndex, objAddr, numBytes)
      _objects(objId) = obj
      objAddr += numBytes
    }
  }
}


