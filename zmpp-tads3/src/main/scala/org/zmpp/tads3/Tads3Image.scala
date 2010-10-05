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

// Static objects are created from the image's static object block
class StaticObject(tads3Image: Tads3Image, val id: Int, val metaClassIndex: Int,
                   val dataAddress: Int, val dataSize: Int) {
  def superClassCount        = tads3Image.memory.shortAt(dataAddress)
  def loadImagePropertyCount = tads3Image.memory.shortAt(dataAddress + 2)
  def objectFlags            = tads3Image.memory.shortAt(dataAddress + 4)
  def superClassIdAt(index: Int)  = tads3Image.memory.shortAt(dataAddress + 6 + index * 4)

  // TODO: The header alignment still does not work !!! We read wrong properties
  private def propertyOffset = dataAddress + 6 + superClassCount * 4

  private def propertyAddressAt(index: Int) = {
    import Tads3Constants._
    propertyOffset + (SizeDataHolder + SizePropertyId) * index
  }
  private def propertyIdAt(index: Int) = {
    tads3Image.memory.shortAt(propertyAddressAt(index))
  }
  private def propertyTypeAt(index: Int) = {
    tads3Image.memory.byteAt(propertyAddressAt(index) + 2)
  }
  private def propertyValueAt(index: Int) = {
    tads3Image.memory.intAt(propertyAddressAt(index) + 3)
  }

  def findProperty(propertyId: Int): Boolean = {
    // First search property in this object, we use a simple linear search for now
    for (i <- 0 until loadImagePropertyCount) {
      val propId = propertyIdAt(i)
      if (propId == propertyId) return true
    }

    // try super class properties
    //println("searching super classes")
    for (i <- 0 until superClassCount) {
      val superClass = tads3Image.objectWithId(superClassIdAt(i))
      if (superClass.findProperty(propertyId)) return true
    }
    false
  }

  override def toString = {
    val result = new StringBuilder
    result.append(
      "[STATIC (%s) size = %d] # super classes: %d, # props: %d FLAGS = %04x\n".format(
        tads3Image.metaClassAtIndex(metaClassIndex).name, dataSize, superClassCount,
        loadImagePropertyCount, objectFlags))
    result.append("Properties:\n")
    for (i <- 0 until loadImagePropertyCount) {
      result.append("  [+%d] - %d: %d %d\n".format(propertyAddressAt(i) - dataAddress,
                                                   propertyIdAt(i),
                                                   propertyTypeAt(i),
                                                   propertyValueAt(i)))
    }
    if (superClassCount > 0) {
    }
    result.toString
  }
}

class MethodHeader(val paramCount: Int, val localCount: Int, val maxStackSlots: Int,
                   val exceptionTableOffset: Int, val debugRecordOffset: Int)

// Define a very simple TADS3 object class for now, which later holds all
// object types. At the moment only static objects
class Tads3Object(staticObject: StaticObject) {
  def findProperty(propertyId: Int) = staticObject.findProperty(propertyId)
  def dump {
    printf("TADS3 OBJECT: %s\n", staticObject.toString)
  }
}

/**
 * Quickly construct a TADS3 image from a memory object.
 * Quite a bit of data is loaded on demand.
 */
class Tads3Image(val memory: Memory) {
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
  
  def startEntryPoint  = memory.intAt(_entp.dataAddress)
  def methodHeaderSize = memory.shortAt(_entp.dataAddress + 4)
  def exTableEntrySize = memory.shortAt(_entp.dataAddress + 6)
  def startAddress     = startEntryPoint + methodHeaderSize
  
  def isValid: Boolean = {
    for (i <- 0 until Tads3Header.Id.length) {
      if (memory.byteAt(i) != Tads3Header.Id(i)) return false
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
  
  def methodHeaderAt(offset: Int) = {
    val addr = _constantPools(PoolTypes.ByteCode).addressForOffset(offset)
    new MethodHeader(memory.byteAt(addr), memory.shortAt(addr + 2),
                     memory.shortAt(addr + 4), memory.shortAt(addr + 6),
                     memory.shortAt(addr + 8))
  }

  def metaClassAtIndex(index: Int) = _metaClasses(index)
  def objectWithId(id: Int)    = {
    new Tads3Object(_objects(id))
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
    val poolId    = memory.shortAt(blockHeader.dataAddress)
    val pageIndex = memory.intAt(blockHeader.dataAddress + 2)
    val xor       = memory.byteAt(blockHeader.dataAddress + 6)
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
  
  private def readMetaclassDependencies(blockHeader: BlockHeader) {
    val numEntries = memory.shortAt(blockHeader.dataAddress)
    _metaClasses = new Array[MetaClass](numEntries)
    printf("# meta classes found: %d\n", numEntries)
    var addr = blockHeader.dataAddress + 2
    for (i <- 0 until numEntries) {
      val entrySize = memory.shortAt(addr)
      val numEntryNameBytes = memory.byteAt(addr + 2)
      val namebuffer = new StringBuilder
      for (j <- 0 until numEntryNameBytes) {
        namebuffer.append(memory.byteAt(addr + 3 + j).asInstanceOf[Char])
      }
      val numPropertyIds = memory.shortAt(addr + 3 + numEntryNameBytes)
      _metaClasses(i) = new MetaClass(namebuffer.toString, numPropertyIds)
      printf("Metaclass %d: %s\n", i, _metaClasses(i).name)
      val propbase = addr + 3 + numEntryNameBytes + 2
      for (j <- 0 until numPropertyIds) {
        _metaClasses(i).propertyIds(j) = memory.shortAt(propbase + j * 2)
        // DEBUGGING
        if (_metaClasses(i).propertyIds(j) == 1579) {
          printf("YEAH, THE ID IS HERE !!")
        }
      }
      addr += entrySize
    }
  }
  
  private def readFunctionSetDependencies(blockHeader: BlockHeader) {
    val numEntries = memory.shortAt(blockHeader.dataAddress)
    _functionSets = new Array[String](numEntries)
    var addr = blockHeader.dataAddress + 2
    for (i <- 0 until numEntries) {
      val numBytes = memory.byteAt(addr)
      val buffer = new StringBuilder
      for (j <- 0 until numBytes) {
        buffer.append(memory.byteAt(addr + 1 + j).asInstanceOf[Char])
      }
      _functionSets(i) = buffer.toString
      addr += numBytes + 1
    }    
  }

  private def readStaticObjectBlock(blockHeader: BlockHeader) {
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
      val numBytes = if (isLarge) memory.intAt(objAddr + 4)
                     else memory.shortAt(objAddr + 4)
      //printf("OBJ ID: %d #BYTES: %d\n", objId, numBytes)
      objAddr += (if (isLarge) 8 else 6)
      val obj = new StaticObject(this, objId, metaClassIndex, objAddr, numBytes)
      _objects(objId) = obj
      objAddr += numBytes
    }
  }
}


