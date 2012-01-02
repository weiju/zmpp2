/*
 * Created on 2010/04/01
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
package org.zmpp.glulx

import java.io._
import scala.util.Random
import scala.annotation.switch
import java.util.logging._

import org.zmpp.base._
import org.zmpp.iff._
import org.zmpp.glk._

// ***************************************************************************
// ****
// **** VM state
// ****
// ***************************************************************************

object GlulxVMState {
  val OffsetLocalsPos     = 4
  val OffsetLocalsFormat  = 8
}

/**
 * GlulxVMState captures the internal state of the Glulx system:
 * - story memory
 * - RAM access
 * - stack access
 *  - local variables
 *  - functions
 */
class GlulxVMState extends VMState {
  val logger = Logger.getLogger("glulx")
  private[this] var _story    : Memory = null
  private[this] var _header   : GlulxStoryHeader = null
  
  // Memory setup
  private[this] var _memheap  : MemoryHeap = null
  private[this] var _extMem   : Memory = null
  private[this] var _extEnd   : Int = 0
  private[this] var _extstart : Int = 0 // cached, because accessed frequently
  var runState = VMRunStates.Running
  var stack             : Stack = null
  // Registers
  var pc                      = 0
  var fp                      = 0
  def sp                      = stack.sp
  def sp_=(newpointer: Int)   = stack.sp_=(newpointer)
  def init(story: Memory) {
    _story     = story
    _header    = new GlulxStoryHeader(story)
    stack      = new Stack(_header.stacksize)
    _extstart  = _header.extstart
    _memheap   = new MemoryHeap(_header.endmem)
    pc         = 0
    fp         = 0
    runState   = VMRunStates.Running
    memsize    = header.endmem
    logger.info("VM INITIALIZED WITH EXT_START: %d END_MEM: %d".format(
                _extstart, header.endmem))
  }
  def restart(originalRam: Array[Byte],
              protectionStart: Int, protectionLength: Int) {
    logger.info("@restart (start: %02x, # bytes: %02x)".format(
                protectionStart, protectionLength))
    logger.info("HEAP ACTIVE: %b EXT_START: $%02x EXT_END: $%02x".format(
                _memheap.active, _extstart, _extEnd))
    stack      = new Stack(_header.stacksize)
    _memheap   = new MemoryHeap(_header.endmem)
    memsize    = header.endmem
    pc         = 0
    fp         = 0
    runState   = VMRunStates.Running
    
    // reset bytes in ram
    val ramsize = _extstart - _header.ramstart
    protectedMemRestore(originalRam, 0, _header.ramstart, ramsize,
                        protectionStart, protectionLength)
  }
  
  private def protectedMemRestore(memarray: Array[Byte], srcOffset: Int,
                                  destOffset: Int, numBytes: Int,
                                  protectionStart: Int,
                                  protectionLength: Int) {
    if (protectionLength > 0) {
      for (i <- 0 until numBytes) {
        val destAddress = destOffset + i
        if (destAddress < protectionStart ||
            destAddress >= protectionStart + protectionLength) {
          _story.setByteAt(destAddress, memarray(i))
        }
      }
    } else {
      _story.copyBytesFrom(memarray, srcOffset, destOffset, numBytes)
    }
  }

  private def _setExtendedMem(size: Int) {
    val currentExtSize = if (_extMem == null) 0 else _extEnd - _extstart
    // note: we actually do not need to "shrink" extended memory, we only
    // need to extend it
    if (currentExtSize != size) {
      if (size > currentExtSize) {
        val extbytes = new Array[Byte](size)
        if (_extMem != null) {
          _extMem.copyBytesTo(extbytes, _extstart, currentExtSize)
        }
        _extMem = new DefaultMemory(extbytes, _extstart)
      }
    }
    _extEnd = _extstart + size
  }

  def story        = _story
  def header       = _header
  def heapIsActive = _memheap.active
  def ramSize      = memsize - header.ramstart

  private def inStoryMem(addr: Int) = addr < _extstart
  private def inExtMem(addr: Int) =
    addr >= _extstart && addr < _extEnd
  private def fitsInStoryMem(addr: Int, size: Int) = {
    inStoryMem(addr) && inStoryMem(addr + size - 1)
  }
  private def fitsOnHeap(addr: Int, size: Int) =
    _memheap.memblockAt(addr) != null

  // Stack interface
  def empty     = stack.empty
  def localsPos = stack.getInt(fp + GlulxVMState.OffsetLocalsPos)
  def frameLen  = stack.getInt(fp)

  def pushByte(value : Int)  = stack.pushByte(value)
  def topByte : Int          = stack.topByte
  def popByte : Int          = {
    if (sp <= fp + frameLen)
      throw new IllegalStateException("POP BYTE - STACK UNDERFLOW !!")
    stack.popByte
  }
  def pushShort(value: Int)  = stack.pushShort(value)
  def topShort : Int         = stack.topShort
  def popShort : Int         = {
    if (sp <= fp + frameLen)
      throw new IllegalStateException("POP SHORT - STACK UNDERFLOW !!")
    stack.popShort
  }
  def pushInt(value : Int)   = stack.pushInt(value)
  def topInt : Int           = stack.topInt
  def popInt : Int           = {
    if (sp <= fp + frameLen)
      throw new IllegalStateException("POP INT - STACK UNDERFLOW !!")
    stack.popInt
  }

  def setIntInStack(addr: Int, value: Int)  = stack.setInt(addr, value)
  def getIntInStack(addr: Int)              = stack.getInt(addr)
  def setByteInStack(addr: Int, value: Int) = stack.setByte(addr, value)
  def getByteInStack(addr: Int)             = stack.getByte(addr)
  def numStackValuesInCallFrame = (sp - (fp + frameLen)) / 4
  
  // special stack functions
  def stackSwap {
    if (numStackValuesInCallFrame < 2)
      throw new IllegalStateException("STACK SWAP - NOT ENOUGH STACK VALUES")
    val val0 = stack.popInt
    val val1 = stack.popInt
    stack.pushInt(val0)
    stack.pushInt(val1)
  }
  def stackPeek(i: Int): Int = {
    if (numStackValuesInCallFrame <= i) {
      throw new IllegalStateException("STACK PEEK - NOT ENOUGH STACK VALUES")
    }
    stack.getInt(sp - (4 * (i + 1)))
  }
  def stackRoll(numValues: Int, numRotatePlaces: Int) {
    if (numRotatePlaces == 0) return
    val tmparr = new Array[Int](numValues)
    for (i <- 0 until numValues) {
      var pos = ((numValues - 1 - i) + numRotatePlaces) % numValues
      if (pos < 0) pos = numValues + pos
      tmparr(pos) = stack.popInt
    }
    for (i <- 0 until numValues) stack.pushInt(tmparr(i))
  }

  // Memory interface
  def memByteAt    (addr: Int) : Int = {
    if      (inStoryMem(addr)) _story.byteAt(addr)
    else if (inExtMem(addr))   _extMem.byteAt(addr)
    else                       _memheap.byteAt(addr)
  }
  def setMemByteAt (addr: Int, value: Int) {
    if (addr < header.ramstart) {
      logger.warning("SETTING BYTE VALUE IN ROM %02x = %d !".format(addr,
                                                                    value))
    }
    if      (inStoryMem(addr)) _story.setByteAt(addr, value)
    else if (inExtMem(addr))   _extMem.setByteAt(addr, value)
    else                       _memheap.setByteAt(addr, value)
  }
  def memShortAt   (addr: Int) : Int = {
    if      (inStoryMem(addr)) _story.shortAt(addr)
    else if (inExtMem(addr))   _extMem.shortAt(addr)
    else                       _memheap.shortAt(addr)
  }
  def setMemShortAt(addr: Int, value: Int) {
    if (addr < header.ramstart) {
      logger.warning("SETTING SHORT VALUE IN ROM %02x = %d !".format(addr,
                                                                     value))
    }
    if      (inStoryMem(addr)) _story.setShortAt(addr, value)
    else if (inExtMem(addr))   _extMem.setShortAt(addr, value) 
    else                       _memheap.setShortAt(addr, value)
  }
  def memIntAt     (addr: Int) : Int = {
    if      (inStoryMem(addr)) _story.intAt(addr)
    else if (inExtMem(addr))   _extMem.intAt(addr)
    else                       _memheap.intAt(addr)
  }
  def setMemIntAt  (addr: Int, value: Int) {
    if (addr < header.ramstart) {
      logger.warning("SETTING INT VALUE IN ROM %02x = %d !".format(addr,
                                                                   value))
    }
    if      (inStoryMem(addr)) _story.setIntAt(addr, value)
    else if (inExtMem(addr))   _extMem.setIntAt(addr, value)
    else                       _memheap.setIntAt(addr, value)
  }

  def ramByteAt    (address : Int) : Int =
    memByteAt(header.ramstart + address)
  def setRamByteAt (address : Int, value : Int) =
    setMemByteAt(header.ramstart + address, value)
  def ramShortAt   (address : Int) : Int =
    memShortAt(header.ramstart + address)
  def setRamShortAt(address : Int, value : Int) =
    setMemShortAt(header.ramstart + address, value)
  def ramIntAt     (address : Int) : Int = memIntAt(header.ramstart + address)
  def setRamIntAt  (address : Int, value : Int) =
    setMemIntAt(header.ramstart + address, value)

  def malloc(size: Int) = _memheap.allocate(size)
  def mfree(addr: Int)  = _memheap.free(addr)
  
  def mcopy(numBytes: Int, srcAddr: Int, destAddr: Int) {
    if (fitsInStoryMem(srcAddr, numBytes) &&
        fitsInStoryMem(destAddr, numBytes)) {
      _story.copyBytesTo(destAddr, srcAddr, numBytes)
    }
    else if (fitsOnHeap(srcAddr, numBytes) && fitsOnHeap(destAddr, numBytes)) {
      _memheap.copyBytesTo(destAddr, srcAddr, numBytes)
    } else {
      for (i <- 0 until numBytes) {
        setMemByteAt(destAddr + i, memByteAt(srcAddr + i))
      }
    }
  }
  def mzero(numBytes: Int, addr: Int) {
    for (i <- 0 until numBytes) setMemByteAt(addr + i, 0)
  }
  def memsize   = if (_memheap.active) _memheap.maxAddress else _extEnd
  def memsize_=(newSize: Int) = _setExtendedMem(newSize - _extstart)
  def heapStart = if (_memheap.active) _extEnd else 0

  // Pushes a call stub, given an operand
  def pushCallStub(storeLocation : Operand) {
    pushCallStub(DestTypes.fromAddressMode(storeLocation.addressMode),
                 storeLocation.value, pc, fp)
  }
  def pushCallStub(destType: Int, destAddr: Int) {
    pushCallStub(destType, destAddr, pc, fp)
  }

  // generic call stub pushing, can take other values for pc and fp
  def pushCallStub(destType: Int, destAddr: Int, pcVal: Int, fpVal: Int) {
    stack.pushInt(destType)
    stack.pushInt(destAddr)
    stack.pushInt(pcVal)
    stack.pushInt(fpVal)
  }
  
  def popCallStubThrow(retval: Int) {
    fp           = stack.popInt
    pc           = stack.popInt
    val destAddr = stack.popInt
    val destType = stack.popInt
    storeResult(destType, destAddr, retval)
  }

  private def setValueInStack(index: Int, vartype: Int, value: Int) {
    (vartype: @switch) match {
      case 1 => // Types.ByteType
        stack.setByte(index, value)
      case 2 => // Types.ShortType
        stack.setShort(index, value)
      case 4 => // Types.IntType
        stack.setInt(index, value)
      case _ =>
        throw new IllegalStateException("unknown local type: " + vartype)
    }
  }
  
  def storeResult(destType: Int, destAddress: Int, value: Int) {
    (destType: @switch) match {
      case 0 => // DestTypes.DoNotStore, do nothing
      case 1 => setMemIntAt(destAddress, value) // DestTypes.Memory
      case 2 => setLocalAtAddress(destAddress, value) // DestTypes.LocalVariable
      case 3 => pushInt(value) // DestTypes.Stack
      case 4 => setRamIntAt(destAddress, value) // DestTypes.Ram
      case _ => 
        throw new IllegalArgumentException(
          "unsupported dest type for store: " + destType)
    }
  }

  // ***********************************************************************
  // ***** Local variable access
  // *********************************
  // determine the current index of the specified local variable relative
  // to the current frame pointer. By returning values, relative to the current
  // frame, we facilitate debugging  by only needing to look at the current
  // frame.
  // we do this by searching the format of the locals and adding pad counts
  // if necessary
  private def alignAddress(address : Int, datatype: Int) : Int = {
    // set to multiple of 4 for ltype == 4 and to even for ltype == 2
    if (datatype == Types.IntType && ((address & 0x03) != 0)) {
      address + (Types.SizeInt - (address & 0x03));
    } else if (datatype == Types.ShortType && ((address & 0x01) != 0)) {
      address + Types.SizeByte;
    } else {
      address
    }
  }

  /**
   * Stores an int at the specified address within the current frame's local
   * space. Note that we do not check the type, simply set the value without
   * a check with type Int. A couple of games seem to rely on the interpreter
   * letting them to write to locals that do not exist.
   */
  def setLocalAtAddress(destAddr: Int, value: Int) {
    stack.setInt(fp + localsPos + destAddr, value)
  }
  /**
   * Analogous to setLocalAtAddress(), this returns an int-sized value
   * and does not check the format, as the Glulx specification requests.
   */
  def getLocalAtAddress(localAddr : Int): Int = {
    stack.getInt(fp + localsPos + localAddr)
  }
  
  // For copyb/copys
  def getLocalByteAtAddress(localAddr : Int): Int = {
    stack.getByte(fp + localsPos + localAddr)
  }
  def getLocalShortAtAddress(localAddr : Int): Int = {
    stack.getShort(fp + localsPos + localAddr)
  }
  def setLocalByteAtAddress(destAddr: Int, value: Int) {
    stack.setByte(fp + localsPos + destAddr, value)
  }
  def setLocalShortAtAddress(destAddr: Int, value: Int) {
    stack.setShort(fp + localsPos + destAddr, value)
  }

  // ***********************************************************************
  // ***** Access local variables through variable numbers
  // *************************************************************

  private def localFrameIndex(localNum: Int) : Int = {
    var descriptorPos    = fp + GlulxVMState.OffsetLocalsFormat
    var currentLocalPos  = localsPos
    var localRangeStart    = 0
    var hasMoreDescriptors = true

    while (hasMoreDescriptors) {
      val ltype    = stack.getByte(descriptorPos)
      val nlocals  = stack.getByte(descriptorPos + 1)
      // needs alignment of localPos ?
      descriptorPos = alignAddress(descriptorPos, ltype)
      // the variable is within the current range, determine which one
      // and returns its address
      if (localRangeStart + nlocals > localNum) {
        return currentLocalPos + (localNum - localRangeStart) * ltype
      }
      // adjust variables
      hasMoreDescriptors = ltype != 0 && nlocals != 0
      currentLocalPos    += ltype * nlocals // advance to next variable block
      localRangeStart    += nlocals         // advance range index
      // advance to next descriptor pair
      descriptorPos      += GlulxVM.SizeLocalDescriptor
    }
    throw new IllegalStateException("unknown local variable: " + localNum)
  }

  private def localType(localNum: Int) : Int = {
    var descriptorPos = fp + GlulxVMState.OffsetLocalsFormat
    var localRangeStart    = 0
    var hasMoreDescriptors = true

    while (hasMoreDescriptors) {
      val ltype    = stack.getByte(descriptorPos)
      val nlocals  = stack.getByte(descriptorPos + 1)
      if (localRangeStart + nlocals > localNum) return ltype
      hasMoreDescriptors = ltype != 0 && nlocals != 0

      // advance to next descriptor pair
      descriptorPos   += GlulxVM.SizeLocalDescriptor
      localRangeStart += nlocals
    }
    0
  }

  /**
   * Sets a specific local variable.
   */
  def setLocal(localNum : Int, value : Int) {
    val ltype  = localType(localNum)
    if (ltype != 0) {
      val lindex = localFrameIndex(localNum)
      // Note: Only set a local if it exists !!!
      setValueInStack(fp + lindex, ltype, value)
    }
  }

  // do not implement if for now  
  def verify: Int = 0

  override def toString = {
    val builder = new StringBuilder
    builder.append("pc = $%02x stackframe = $%02x\n".format(pc, fp))
    builder.append(stack.toStringFrom(fp))
    builder.toString
  }
  
  def stackValuesAsString = {
    val builder = new StringBuilder
    val stackStart = fp + frameLen
    val numElems = numStackValuesInCallFrame
    builder.append("[")
    for (i <- 0 until numElems) {
      if (i > 0) builder.append(", ")
      builder.append("#$%02x".format(stack.getInt(stackStart + (i * 4))))
    }
    builder.append("]")
    builder.toString
  }
  
  // Reading data at the PC
  def nextByte: Int  = {
    pc += 1
    memByteAt(pc - 1)
  }
  def nextShort: Int = {
    pc += 2
    memShortAt(pc - 2)
  }
  def nextInt: Int   = {
    pc += 4
    memIntAt(pc - 4)
  }

  // ***********************************************************************
  // ***** State Serialization
  // *************************************************************
  def readSnapshot(snapshot: Snapshot, protectionStart: Int,
                   protectionLength: Int) {
    val ramsize = _extstart - header.ramstart
    protectedMemRestore(snapshot.ram, 0, _header.ramstart, ramsize,
                        protectionStart, protectionLength)
    stack.initFromByteArray(snapshot.stack)
    
    // TODO: Extmem and Heap
  }
  
  def createSnapshot(storeLocation: Operand): Snapshot = {
    val ram         = cloneRam
    logger.info("CREATE_SNAPSHOT, PC = $%02x FP = %d SP: %d".format(pc,
                                                                    fp, sp))
    pushCallStub(storeLocation)
    val stackValues = stack.cloneValues
    val extmem: Array[Byte] = null

    // TODO: extmem and heap
    new Snapshot(ram, stackValues, extmem)
  }

  def cloneRam = {
    val ramsize = _extstart - header.ramstart
    logger.info("Copying %d Bytes of RAM to preserve initial data".format(ramsize))
    val ram = new Array[Byte](ramsize)
    _story.copyBytesTo(ram, header.ramstart, ramsize)
    ram
  }  
}

/****************************************************************************
 ****
 **** VM main control
 ****
 */
object GlulxVM {
  val MaxLocalDescriptors = 255
  val MaxOperands         = 10
  val MaxArguments        = 20
  val SizeLocalDescriptor = Types.SizeByte * 2  
}

/**
 * GlulxVM captures the logic aspect of the Glulx virtual machine.
 */
class GlulxVM {
  val logger = Logger.getLogger("glulx")
  var iterations = 1

  // State is public for subsystems to access. This is the only data
  // that is needed to be serializaed between turns. All other data
  // is scratch data which is only valid during one turn.
  val state = new GlulxVMState

  // Cached currrent frame state. This is used in setting up the current
  // function and is only supposed to be used at that time. When returning
  // from a function, the descriptors are not restored, so do not rely on them !
  // Note: This is mainly an optimization to avoid reading the state over and
  // over again, we might fall back to just reading from memory and pass the
  // state.
  private[this] val _localDescriptors =
    new Array[LocalDescriptor](GlulxVM.MaxLocalDescriptors)
  
  // quick and easy access to the current instruction's values, so we
  // do not need to read them over and over
  private[this] val _operands           = new Array[Operand](GlulxVM.MaxOperands)
  private[this] var _opcodeNum          = 0
  private[this] var _opcodeNumSize      = 0
  
  // function arguments - we avoid creating them over and over
  // we are using these when we setup a normal function call and
  // we also use them in accelerated functions
  private[this] val _arguments          = new Array[Int](GlulxVM.MaxArguments)
  private[this] var _numArguments       = 0
  
  // VM state
  private[this] var _glk        : Glk         = null
  private[this] var _glkDispatch: GlkDispatch = null
  private[this] val _random                   = new Random

  private[this] var _undoSnapshots: List[Snapshot] = Nil
  private[this] val _accelSystem              = new AccelSystem(this)
  
  // The original state of writable memory after loading
  private[this] var _originalRam: Array[Byte] = null
  private[this] var _protectionStart  = 0
  private[this] var _protectionLength = 0

  // IO Systems
  def eventManager = _glk.eventManager
  var blorbData : BlorbData = null
  var currentDecodingTable = 0
  var currentIOSystem: IOSystem  = new NullIOSystem(this, 0)
  def memIntAt(addr: Int): Int   = state.memIntAt(addr)
  def memByteAt(addr: Int): Int  = state.memByteAt(addr)
  def memShortAt(addr: Int): Int = state.memShortAt(addr)

  // initialization
  for (i <- 0 to GlulxVM.MaxLocalDescriptors - 1)
    _localDescriptors(i) = new LocalDescriptor
  for (i <- 0 to GlulxVM.MaxOperands - 1) _operands(i) = new Operand
  
  def init(aStory: Memory, aBlorbData: BlorbData) {
    blorbData = aBlorbData
    _glk = new Glk(new EventManager(state))
    _glkDispatch = new GlkDispatch(state, _glk)
    state.init(aStory)
    currentDecodingTable = state.header.decodingTable
    _accelSystem.glk      = _glk
    if (_originalRam == null) _originalRam = state.cloneRam

    prepareCall(state.header.startfunc, null)
  }

  def screenUI = _glk.screenUI
  def screenUI_=(screenUI: GlkScreenUI) = _glk.screenUI = screenUI
  def nativeSoundSystem = _glk.nativeSoundSystem
  def nativeSoundSystem_=(soundSystem: NativeSoundSystem) = _glk.nativeSoundSystem = soundSystem

  
  def runState   = state.runState
  def header     = state.header
  
  private def restart {
    state.restart(_originalRam, _protectionStart, _protectionLength)
    //_protectionStart  = 0
    //_protectionLength = 0
    //_undoSnapshots = Nil

    prepareCall(state.header.startfunc, null)
  }

  private def printState = println(state.toString)
  
  private def readLocalDescriptors(addr : Int) = {
    var currentAddr = addr
    var descIndex = 0
    var hasMoreDescriptors = true
    while (hasMoreDescriptors) {
      _localDescriptors(descIndex).localType  = state.memByteAt(currentAddr)
      _localDescriptors(descIndex).localCount =
        state.memByteAt(currentAddr + 1)

      hasMoreDescriptors = !(_localDescriptors(descIndex).localType == 0 &&
                             _localDescriptors(descIndex).localCount == 0)
      descIndex += 1
      currentAddr += GlulxVM.SizeLocalDescriptor
    }
    // include the terminating pair in the count
    descIndex
  }

  private def setLocalDescriptorsToCallFrame(numDescriptors : Int) = {
    var i = 0
    while (i < numDescriptors) {
      state.pushByte(_localDescriptors(i).localType)
      state.pushByte(_localDescriptors(i).localCount)
      i += 1
    }
    // Ensure a size dividable by 4 (the size of an int)
    var localDescriptorSize = numDescriptors * Types.SizeShort
    if ((localDescriptorSize % Types.SizeInt) != 0) {
      state.pushShort(0)
      localDescriptorSize += Types.SizeShort
    }
    localDescriptorSize
  }
  
  // returns the size of the locals sections
  private def setLocalsToCallFrame(numDescriptors : Int) : Int = {
    var localSectionSize = 0
    // we subtract 1 from numDescriptors, because we do not include the
    // terminator
    var i = 0
    while (i < numDescriptors - 1) {
      val numlocals = _localDescriptors(i).localCount
      val ltype     = _localDescriptors(i).localType
      if (!Types.isValidType(ltype)) {
        throw new IllegalArgumentException("unknown local type: " + ltype)
      }
      // Padding: For short, pad to even address, for int, pad to multiple
      // of 4
      var numPadBytes = 0
      if (ltype == Types.ShortType && ((state.sp & 0x01) == 1)) {
        state.pushByte(0)
        numPadBytes = 1
      } else if (ltype == Types.IntType &&
                 ((state.sp & 0x03) != 0)) {
        numPadBytes = Types.SizeInt - (state.sp & 0x03)
        var j = 0
        while (j < numPadBytes) {
          state.pushByte(0)
          j += 1
        }
      }
      // push numlocals locals of size ltype on the stack, we do this
      // by incrementing the stackpointer, which does not do any initialization
      // to the variables
      val blocksize = numlocals * ltype
      var j = 0
      while (j < blocksize) {
        state.pushByte(0)
        j += 1
      }
      localSectionSize += blocksize + numPadBytes
      i += 1
    }
    localSectionSize
  }
  
  private def decodeOpcodeNum {
    // look at the two highest bits: 11 means 4 bytes, 10 means 2 bytes
    // else one byte
    val b0 = state.memByteAt(state.pc) & 0xff
    val bitpattern = b0 & 0xc0

    if (bitpattern == 0xc0) {
      _opcodeNum = state.memIntAt(state.pc) - 0xc0000000
      _opcodeNumSize = Types.SizeInt
    } else if (bitpattern == 0x80) {
      _opcodeNum = (state.memShortAt(state.pc) & 0xffff) - 0x8000
      _opcodeNumSize = Types.SizeShort
    } else {
      _opcodeNum = b0
      _opcodeNumSize = Types.SizeByte
    }
    state.pc += _opcodeNumSize
  }
  
  private def readOperand(addressMode : Int) = {
    (addressMode: @switch) match {
      case 0  => 0               // ConstZero
      case 1  => state.nextByte  // ConstByte
      case 2  => state.nextShort // ConstShort
      case 3  => state.nextInt   // ConstInt
      case 5  => state.nextByte  // Address00_FF
      case 6  => state.nextShort // Address0000_FFFF
      case 7  => state.nextInt   // AddressAny
      case 8  => 0               // Stack
      case 9  => state.nextByte  // Local00_FF
      case 10 => state.nextShort // Local0000_FFFF
      case 11 => state.nextInt   // LocalAny
      case 13 => state.nextByte  // Ram00_FF
      case 14 => state.nextShort // Ram0000_FFFF
      case 15 => state.nextInt   // RamAny
      case _ =>
        throw new IllegalArgumentException("unsupported address mode: " +
                                           addressMode)
    }
  }

  private def readOperands {
    val addrModeOffset = state.pc
    val numOperands = Opcodes.numOperands(_opcodeNum)
    val nbytesNumOperands = numOperands / 2 + numOperands % 2
    state.pc += nbytesNumOperands // adjust pc to the start of operand data
    var numRead = 0
    var i = 0
    while (i < nbytesNumOperands) {
      val byteVal = state.memByteAt(addrModeOffset + i)
      _operands(numRead).addressMode = byteVal & 0x0f
      _operands(numRead).value = readOperand(_operands(numRead).addressMode)

      numRead += 1
      if (numRead < numOperands) {
        _operands(numRead).addressMode = (byteVal >>> 4) & 0x0f
        _operands(numRead).value = readOperand(_operands(numRead).addressMode)
        numRead += 1
      }
      i += 1
    }
  }
  
  private def getOperand(pos : Int) :Int = {
    (_operands(pos).addressMode: @switch) match {
      case 0  => 0 // ConstZero
      case 1  => Types.signExtend8(_operands(pos).value) // ConstByte
      case 2  => Types.signExtend16(_operands(pos).value) // ConstShort
      case 3  => _operands(pos).value // ConstInt
      case 7  => state.memIntAt(_operands(pos).value) // AddressAny
      case 8  => state.popInt // Stack
      case 9  => // Local00_FF
        state.getLocalAtAddress(_operands(pos).value)
      case 10  => // Local0000_FFFF
        state.getLocalAtAddress(_operands(pos).value)
      case 11 => // LocalAny
        state.getLocalAtAddress(_operands(pos).value)
      case 13 => state.ramIntAt(_operands(pos).value)
      case 14 => state.ramIntAt(_operands(pos).value)
      case 15 => state.ramIntAt(_operands(pos).value)
      case _ =>
        throw new IllegalStateException("unsupported operand type: " +
          _operands(pos).addressMode)
    }
  }

  /*
   * Used by signed instructions. Only an alias, since getOperand() delivers
   * signed values anyways. Some instructions use it more for illustrative
   * purposes.
   */
  private def getSignedOperand(pos: Int) : Int = getOperand(pos)

  // only for copyb/copys
  // Only used by copyb.
  private def getOperand8(pos : Int) :Int = {
    (_operands(pos).addressMode: @switch) match {
      case 0  => 0 // ConstZero
      case 1  => Types.signExtend8(_operands(pos).value) // ConstByte
      case 2  => Types.signExtend16(_operands(pos).value) // ConstShort
      case 3  => _operands(pos).value // ConstInt
      case 5  => // Address00_FF
        state.memByteAt(_operands(pos).value)
      case 6  => // Address0000_FFFF
        state.memByteAt(_operands(pos).value)
      case 7  => // AddressAny
        state.memByteAt(_operands(pos).value)
      case 8  => state.popInt // Stack
      case 9  => // Local00_FF 
        state.getLocalByteAtAddress(_operands(pos).value)
      case 10 => // Local0000_FFFF
        state.getLocalByteAtAddress(_operands(pos).value)
      case 11 => // LocalAny
        state.getLocalByteAtAddress(_operands(pos).value)
      case 13 => // Ram00_FF
        state.ramByteAt(_operands(pos).value)
      case 14 => // Ram0000_FFFF
        state.ramByteAt(_operands(pos).value)
      case 15 => // RamAny
        state.ramByteAt(_operands(pos).value)
      case _         =>
        throw new IllegalStateException("unsupported operand type: " +
          _operands(pos).addressMode)
    }
  }
  
  // Only used by copys.
  private def getOperand16(pos : Int) :Int = {
    (_operands(pos).addressMode: @switch) match {
      case 0  => 0 // ConstZero
      case 1  => Types.signExtend8(_operands(pos).value) // ConstByte
      case 2  => Types.signExtend16(_operands(pos).value) // ConstShort
      case 3  => _operands(pos).value // ConstInt
      case 5  => // Address00_FF
        state.memShortAt(_operands(pos).value)
      case 6  => // Address0000_FFFF
        state.memShortAt(_operands(pos).value)
      case 7  => // AddressAny
        state.memShortAt(_operands(pos).value)
      case 8  => state.popInt // Stack
      case 9  => // Local00_FF
        state.getLocalShortAtAddress(_operands(pos).value)
      case 10 => // Local0000_FFFF
        state.getLocalShortAtAddress(_operands(pos).value)
      case 11 => // LocalAny
        state.getLocalShortAtAddress(_operands(pos).value)
      case 13 => // Ram00_FF
        state.ramShortAt(_operands(pos).value)
      case 14 => // Ram0000_FFFF
        state.ramShortAt(_operands(pos).value)
      case 15 => // RamAny
        state.ramShortAt(_operands(pos).value)
      case _         =>
        throw new IllegalStateException("unsupported operand type: " +
          _operands(pos).addressMode)
    }
  }
  // ***********************************************************************
  // ***** Storing
  // *********************************
  private def storeAtOperand(operand: Operand, value : Int) {
    (operand.addressMode: @switch) match {
      case 0  => // ConstZero, throw result away
      case 5  => // Address00_FF
        state.setMemIntAt(operand.value, value)
      case 6  => // Address0000_FFFF
        state.setMemIntAt(operand.value, value)
      case 7  => // AddressAny
        state.setMemIntAt(operand.value, value)
      case 8  => state.pushInt(value) // Stack
      case 9  => // Local00_FF
        state.setLocalAtAddress(operand.value, value)
      case 10 => // Local0000_FFFF
        state.setLocalAtAddress(operand.value, value)
      case 11 => // LocalAny
        state.setLocalAtAddress(operand.value, value)
      case 13 => // Ram00_FF
        state.setRamIntAt(operand.value, value)
      case 14 => // Ram0000_FFFF
        state.setRamIntAt(operand.value, value)
      case 15 => // RamAny
        state.setRamIntAt(operand.value, value)
      case _ =>
        throw new IllegalArgumentException(
          "unsupported address mode for store: " + operand.addressMode)
    }
  }
  private def storeAtOperand(pos : Int, value : Int) {
    storeAtOperand(_operands(pos), value)
  }

  // Only used by copyb.
  private def storeAtOperand8(pos : Int, value : Int) {
    (_operands(pos).addressMode: @switch) match {
      case 0  => // ConstZero, throw result away
      case 5  => // Address00_FF
        state.setMemByteAt(_operands(pos).value, value)
      case 6  => // Address0000_FFFF
        state.setMemByteAt(_operands(pos).value, value)
      case 7  => // AddressAny
        state.setMemByteAt(_operands(pos).value, value)
      case 8  => state.pushInt(value) // Stack
      case 9  => // Local00_FF
        state.setLocalByteAtAddress(_operands(pos).value, value)
      case 10 => // Local0000_FFFF
        state.setLocalByteAtAddress(_operands(pos).value, value)
      case 11 => // LocalAny
        state.setLocalByteAtAddress(_operands(pos).value, value)
      case 13 => // Ram00_FF
        state.setRamByteAt(_operands(pos).value, value)
      case 14 => // Ram0000_FFFF
        state.setRamByteAt(_operands(pos).value, value)
      case 15 => // RamAny
        state.setRamByteAt(_operands(pos).value, value)
      case _ =>
        throw new IllegalArgumentException(
          "unsupported address mode for store: " + _operands(pos).addressMode)
    }
  }

  // Only used by copys, 16-bit values
  private def storeAtOperand16(pos : Int, value : Int) {
    (_operands(pos).addressMode: @switch) match {
      case 0  => // ConstZero, throw result away
      case 5  => // Address00_FF
        state.setMemShortAt(_operands(pos).value, value)
      case 6  => // Address0000_FFFF
        state.setMemShortAt(_operands(pos).value, value)
      case 7  => // AddressAny
        state.setMemShortAt(_operands(pos).value, value)
      case 8  => state.pushInt(value) // Stack
      case 9  => // Local00_FF
        state.setLocalShortAtAddress(_operands(pos).value, value)
      case 10 => // Local0000_FFFF
        state.setLocalShortAtAddress(_operands(pos).value, value)
      case 11 => // LocalAny
        state.setLocalShortAtAddress(_operands(pos).value, value)
      case 13 => // Ram00_FF
        state.setRamShortAt(_operands(pos).value, value)
      case 14 => // Ram0000_FFFF
        state.setRamShortAt(_operands(pos).value, value)
      case 15 => // RamAny
        state.setRamShortAt(_operands(pos).value, value)
      case _ =>
        throw new IllegalArgumentException(
          "unsupported address mode for store: " + _operands(pos).addressMode)
    }
  }
  
  // ***********************************************************************
  // ***** Functions
  // *********************************
  // decodes specified function: initializes the call frame
  // this does *NOT* push a call stub
  private def callFunction(funaddr : Int) {
    // create call frame (might have to be pushed to the state class)
    state.pushInt(0) // call frame size
    state.pushInt(0) // locals position

    val funtype = state.memByteAt(funaddr) & 0xff
    val numDescriptors = readLocalDescriptors(funaddr + 1)
    val localDescriptorSize = setLocalDescriptorsToCallFrame(numDescriptors)

    // now that we know the size of the local descriptors section, we set
    // the position of locals
    state.setIntInStack(state.fp + GlulxVMState.OffsetLocalsPos,
                         localDescriptorSize + 8)
    val localSectionSize = setLocalsToCallFrame(numDescriptors)
    
    if (funtype == 0xc0) { // stack-arg type
      // push arguments backwards, then the number of arguments
      var i = 0
      while (i < _numArguments) {
        state.pushInt(_arguments(_numArguments - i - 1))
        i += 1
      }
      state.pushInt(_numArguments)
    } else if (funtype == 0xc1) { // local-arg type
      // Copy arguments on the stack backwards to the locals
      var i = 0
      while (i < _numArguments) {
        state.setLocal(i, _arguments(i))
        i += 1
      }
    } else {
      throw new IllegalArgumentException(
        "unsupported function type: %02x".format(funtype))
    }

    // set frame len
    state.setIntInStack(state.fp, state.localsPos + localSectionSize)
    // jump to the code
    state.pc = funaddr + 1 + GlulxVM.SizeLocalDescriptor * numDescriptors
  }
  
  // normal function call, called by the VM itself
  private def prepareCall(funaddr : Int, storeLocation : Operand) {
    if (storeLocation != null) {
      state.pushCallStub(storeLocation)
      state.fp = state.sp
    }
    if (_accelSystem.isAccelerated(funaddr)) {
      // 4 dummy ints on the stack to trick the check
      state.pushInt(0)
      state.pushInt(0)
      state.pushInt(0)
      state.pushInt(0)
      _accelSystem.call(funaddr, _arguments, _numArguments)
    } else {
      callFunction(funaddr)
    }
  }
  
  // generic function call, called by string decoding
  def prepareCall(funaddr : Int, destType: Int, destAddr: Int) {
    state.pushCallStub(destType, destAddr)
    state.fp = state.sp
    callFunction(funaddr)
  }
  
  // called by @tailcall
  private def tailCall(funaddr: Int, numArgs: Int) {
    _numArguments = numArgs
    var i = 0
    while (i < _numArguments) {
      _arguments(i) = state.popInt
      i += 1
    }
    state.sp = state.fp
    prepareCall(funaddr, null)
  }

  /**
   * Implements @callf, @callfi, @callfii, @callfiii.
   */
  private def doCallf(funaddr: Int, storeLocation: Operand, args: Int*) {
    var i = 0
    while (i < args.length) {
      _arguments(i) = args(i)
      i += 1
    }
    _numArguments = args.length
    prepareCall(funaddr, storeLocation)
  }
  
  /**
   * Perform a call given an int array as arguments, this method is called
   * by the I/O system.
   */
  def callWithArgs(destType: Int, destAddr: Int, pcVal: Int, fpVal: Int,
                   funaddr: Int, args: Array[Int]) {
    var i = 0
    while (i < args.length) {
      _arguments(i) = args(i)
      i += 1
    }
    _numArguments = args.length
    state.pushCallStub(destType, destAddr, pcVal, fpVal)
    state.fp = state.sp
    callFunction(funaddr)
  }
  def callWithArgs(destType: Int, destAddr: Int, pcVal: Int, fpVal: Int,
                   funaddr: Int, args: Int*) {
    var i = 0
    while (i < args.length) {
      _arguments(i) = args(i)
      i += 1
    }
    _numArguments = args.length
    state.pushCallStub(destType, destAddr, pcVal, fpVal)
    state.fp = state.sp
    callFunction(funaddr)
  }

  def callWithArgsNoCallStub(funaddr: Int, args: Array[Int]) {
    var i = 0
    while (i < args.length) {
      _arguments(i) = args(i)
      i += 1
    }
    _numArguments = args.length
    state.fp = state.sp
    callFunction(funaddr)
  }
  def callWithArgsNoCallStub(funaddr: Int, args: Int*) {
    var i = 0
    while (i < args.length) {
      _arguments(i) = args(i)
      i += 1
    }
    _numArguments = args.length
    state.fp = state.sp
    callFunction(funaddr)
  }

  // Returns from a function
  def popCallStub(retval: Int) {
    if (state.sp < state.fp + 12) {
      throw new IllegalStateException("popCallStub(), stack is too small !!")
    }
    state.sp = state.fp
    if (state.sp == 0) {
      // return from entry function -> Quit
      state.runState = VMRunStates.Halted
    } else {
      // we can't use GlulxVM's popInt(), because it performs checks on
      // the call frame, which is exactly what we manipulate here
      val fpValue  = state.stack.popInt
      val pcValue  = state.stack.popInt
      val destAddr = state.stack.popInt
      val destType = state.stack.popInt
      if (DestTypes.isStringDestType(destType)) {
        handleStringCallStub(destType, destAddr, pcValue, fpValue)
      } else { // regular behaviour
        state.fp = fpValue
        state.pc = pcValue
        state.storeResult(destType, destAddr, retval)
      }
    }
  }
  def handleStringCallStub(destType: Int, destAddr: Int, pcValue: Int,
                           fpValue: Int) {
    (destType: @switch) match {
      case 10 => // DestTypes.ResumePrintCompressed
        currentIOSystem.streamStr(StreamStrState.resumeAt(pcValue, destAddr))
      case 12 => // DestTypes.ResumePrintDecimal
        currentIOSystem.streamNum(pcValue, destAddr)
      case 13 => // DestTypes.ResumePrintCString
        currentIOSystem.streamStr(StreamStrState.resumeCStringAt(pcValue))
      case 14 => // DestTypes.ResumePrintUnicode
        currentIOSystem.streamStr(StreamStrState.resumeUniStringAt(pcValue))
      case _ => // do nothing
        throw new UnsupportedOperationException(
          "Encountered call stub type: %d".format(destType))
    }
  }

  // ***********************************************************************
  // ***** Strings
  // *********************************
  // ***********************************************************************
  // ***** Branches
  // *********************************
  private def doBranch(offset: Int) {
    if (offset == 0 || offset == 1) popCallStub(offset)
    else state.pc += offset - 2
  }

  // ***********************************************************************
  // ***** Dispatch
  // *********************************
/*
  def executeFyreCall {
    println("executeFyreCall()")
    val code = getOperand(0)
    val operand1 = getOperand(1)
    val operand2 = getOperand(2)
    val operand3 = getOperand(3)
    import FyreCallCodes._
    code match {
      case ReadLine =>
        logger.info("fyrecall.readLine()")
      case SetStyle =>
        logger.info("fyrecall.setStyle()")
      case ToLower =>
        logger.info("fyrecall.toLower()")
      case ToUpper =>
        logger.info("fyrecall.toUpper()")
      case Channel =>
        logger.info("fyrecall.channel()")
      case EnableFilter =>
        logger.info("fyrecall.enableFilter()")
      case ReadKey =>
        logger.info("fyrecall.readKey()")
      case SetVeneer =>
        logger.info("fyrecall.setVeneer()")
      case _ =>
        logger.info("unknown fyrecall: " + code)
    }
  }
*/
  def executeInstruction {
    import Opcodes._
    (_opcodeNum: @switch) match {
      case 0x00  => // nop, do nothing
      case 0x10  => // add
        storeAtOperand(2, getOperand(0) + getOperand(1))
      case 0x11  => // sub
        storeAtOperand(2, getOperand(0) - getOperand(1))
      case 0x12  => // mul
        storeAtOperand(2, getOperand(0) * getOperand(1))
      case 0x13  => // div
        storeAtOperand(2, getSignedOperand(0) / getSignedOperand(1))
      case 0x14  => // mod
        storeAtOperand(2, getSignedOperand(0) % getSignedOperand(1))
      case 0x15  => // neg
        storeAtOperand(1, -(getOperand(0)))
      case 0x18  => // bitand
        storeAtOperand(2, getOperand(0) & getOperand(1))
      case 0x19  => // bitor
        storeAtOperand(2, getOperand(0) | getOperand(1))
      case 0x1a  => // bitxor
        storeAtOperand(2, getOperand(0) ^ getOperand(1))
      case 0x1b  => // bitnot
        storeAtOperand(1, ~getOperand(0))
      case 0x1c  => // shiftl
        val value    = getOperand(0)
        val numShift = getOperand(1)
        val result   = if (numShift >= 32 || numShift < 0) 0
                       else value << numShift
        storeAtOperand(2, result)
      case 0x1d  => // sshiftr
        val value    = getOperand(0)
        val numShift = getOperand(1)
        val result   =
          if (value < 0 && (numShift >= 32 || numShift < 0)) -1
          else if (value >= 0 && (numShift >= 32 || numShift < 0)) 0
          else value >> numShift
        storeAtOperand(2, result)
      case 0x1e  => // ushiftr
        val value    = getOperand(0)
        val numShift = getOperand(1)
        val result   = if (numShift >= 32 || numShift < 0) 0
                       else value >>> numShift
        storeAtOperand(2, result)
      case 0x20  => // jump
        doBranch(getOperand(0))
      case 0x22  => // jz
        if (getSignedOperand(0) == 0) doBranch(getOperand(1))
      case 0x23  => // jnz
        if (getSignedOperand(0) != 0) doBranch(getOperand(1))
      case 0x24  => // jeq
        if (getSignedOperand(0) == getSignedOperand(1)) doBranch(getOperand(2))
      case 0x25  => // jne
        if (getSignedOperand(0) != getSignedOperand(1)) doBranch(getOperand(2))
      case 0x26  => // jlt
        if (getSignedOperand(0) < getSignedOperand(1)) doBranch(getOperand(2))
      case 0x27 => // jge
        if (getSignedOperand(0) >= getSignedOperand(1)) doBranch(getOperand(2))
      case 0x28  => // jgt
        if (getSignedOperand(0) > getSignedOperand(1)) doBranch(getOperand(2))
      case 0x29  => // jle
        if (getSignedOperand(0) <= getSignedOperand(1)) doBranch(getOperand(2))
      case 0x2a => // jltu
        val op0 = getOperand(0).toLong & 0x0ffffffffl
        val op1 = getOperand(1).toLong & 0x0ffffffffl
        if (op0 < op1) doBranch(getOperand(2))
      case 0x2b => // jgeu
        val op0 = getOperand(0).toLong & 0x0ffffffffl
        val op1 = getOperand(1).toLong & 0x0ffffffffl
        if (op0 >= op1) doBranch(getOperand(2))
      case 0x2c  => // jgtu
        val op0 = getOperand(0).toLong & 0x0ffffffffl
        val op1 = getOperand(1).toLong & 0x0ffffffffl
        if (op0 > op1) doBranch(getOperand(2))
      case 0x2d  => // jleu
        val op0 = getOperand(0).toLong & 0x0ffffffffl
        val op1 = getOperand(1).toLong & 0x0ffffffffl
        if (op0 <= op1) doBranch(getOperand(2))
      case 0x30  => // call
        // Take the arguments from the stack and store them, prepareCall
        // will use them and store them according to the function type
        val funaddr = getOperand(0)
        _numArguments = getOperand(1)
        var i = 0
        while (i < _numArguments) {
          _arguments(i) = state.popInt
          i += 1
        }
        prepareCall(funaddr, _operands(2))
      case 0x31  => // return
        popCallStub(getOperand(0))
      case 0x32  => // catch
        // Pull the value from the stack if L1 is SP
        if (_operands(0).addressMode == AddressModes.Stack ||
            _operands(1).addressMode == AddressModes.Stack) {
          val branchOffset = getOperand(1)
          state.pushCallStub(_operands(0))
          storeAtOperand(0, state.sp) // catch token
          doBranch(branchOffset)
        } else {
          state.pushCallStub(_operands(0))
          storeAtOperand(0, state.sp) // catch token
          doBranch(getOperand(1))
        }
      case 0x33  => // throw
        val storeVal   = getOperand(0)
        val catchToken = getOperand(1)
        logger.info("@throw %d %d CURRENT SP = %d".format(storeVal, catchToken,
                                                          state.sp))
        if (state.sp < catchToken)
          throw new IllegalStateException("@throw: catch token > current SP !!!")
        state.sp = catchToken
        logger.info("@throw, SP is now: %d\n".format(state.sp))
        state.popCallStubThrow(storeVal)
        logger.info("@throw, after popCallStub SP is now: %d\n"
                    .format(state.sp))
      case 0x34  => // tailcall
        tailCall(getOperand(0), getOperand(1))
      case 0x40  => // copy
        storeAtOperand(1, getOperand(0))
      case 0x41  => // copys
        storeAtOperand16(1, getOperand16(0) & 0xffff)
      case 0x42  => // copyb
        storeAtOperand8(1, getOperand8(0) & 0xff)
      case 0x44  => // sexs 
        storeAtOperand(1, Types.signExtend16(getOperand(0)))
      case 0x45  => // sexb
        storeAtOperand(1, Types.signExtend8(getOperand(0)))
      case 0x48  => // aload
        val arr   = getOperand(0)
        val index = getSignedOperand(1)
        storeAtOperand(2, state.memIntAt(arr + index * 4))
      case 0x49  => // aloads
        val arr = getOperand(0)
        val index = getSignedOperand(1)
        storeAtOperand(2, state.memShortAt(arr + index * 2))
      case 0x4a  => // aloadb
        val arr    = getOperand(0)
        val index  = getSignedOperand(1)
        storeAtOperand(2, state.memByteAt(arr + index))
      case 0x4b  => // aloadbit
        val addr      = getOperand(0)
        val bitOffset = getSignedOperand(1)
        var memAddr   = addr + bitOffset / 8
        var bitnum    = bitOffset % 8
        if (bitnum < 0) {
          // adjust bitnum if necessary
          memAddr -= 1
          bitnum += 8
        }
        val mask = 1 << bitnum
        val test =
          if ((state.memByteAt(memAddr) & mask) == mask) 1 else 0
        storeAtOperand(2, test)
      case 0x4c  => // astore
        val arr = getOperand(0)
        val index = getSignedOperand(1)
        state.setMemIntAt(arr + index * 4, getOperand(2))
      case 0x4d  => // astores
        val arr = getOperand(0)
        val index = getSignedOperand(1)
        state.setMemShortAt(arr + index * 2, getOperand(2))
      case 0x4e  => // astoreb
        val arr   = getOperand(0)
        val index = getSignedOperand(1)
        state.setMemByteAt(arr + index, getOperand(2))
      case 0x4f  => // astorebit
        val addr      = getOperand(0)
        val bitOffset = getSignedOperand(1)
        var memAddr   = addr + bitOffset / 8
        var bitnum    = bitOffset % 8
        if (bitnum < 0) {
          // adjust bitnum if necessary
          memAddr -= 1
          bitnum += 8
        }
        if (getOperand(2) == 0) { // clear
          state.setMemByteAt(memAddr,
              state.memByteAt(memAddr) & (~(1 << bitnum) & 0xff))
        } else { // set
          state.setMemByteAt(memAddr,
              state.memByteAt(memAddr) | ((1 << bitnum) & 0xff))
        }
      case 0x50  => // stkcount
        storeAtOperand(0, state.numStackValuesInCallFrame)
      case 0x51  => // stkpeek
        storeAtOperand(1, state.stackPeek(getOperand(0)))
      case 0x52  => // stkswap
        state.stackSwap
      case 0x53  => // stkroll
        state.stackRoll(getOperand(0), getSignedOperand(1))
      case 0x54  => // stkcopy
        val numElems = getOperand(0)
        val copyStart = state.sp - Types.SizeInt * numElems
        var i = 0
        while (i < numElems) {
          state.pushInt(state.getIntInStack(copyStart + i * Types.SizeInt))
          i += 1
        }
      case 0x70  => //streamchar
        currentIOSystem.streamChar((getOperand(0) & 0xff).asInstanceOf[Char])
      case 0x71  => // streamnum
        currentIOSystem.streamNum(getSignedOperand(0), 0)
      case 0x72  => // streamstr
        currentIOSystem.streamStr(StreamStrState.newString(getOperand(0)))
      case 0x73  => // streamunichar
        currentIOSystem.streamUniChar(getOperand(0))
      case 0x100 => // gestalt
        val selector = getOperand(0)
        val arg      = getOperand(1)
        if (selector == GlulxGestalt.MAllocHeap) {
          storeAtOperand(2, state.heapStart)
        } else {
          storeAtOperand(2, GlulxGestalt.gestalt(selector, arg))
        }
      case 0x101 => // debugtrap
        fatal("[** ERROR, VM HALTED WITH CODE %d **]".format(getOperand(0)))
      case 0x102 => // getmemsize
        storeAtOperand(0, state.memsize)
      case 0x103 => // setmemsize
        val newSize = getOperand(0)
        logger.info("@setmemsize %d\n".format(newSize))
        if (newSize < header.endmem) fatal("@setmemsize: size must be >= ENDMEM")
        if (newSize % 256 != 0) fatal("@setmemsize: size must be multiple of 256")
        if (state.heapIsActive)
          fatal("@setmemsize: can not set while heap is active")
        state.memsize = newSize
        // Result is 0 for success, 1 for fail
        0
      case 0x104 => // jumpabs
        state.pc = getOperand(0)
      case 0x110 => // random
        val range = getSignedOperand(0)
        if (range < 0) {
          val translate = range + 1
          storeAtOperand(1, _random.nextInt(-range) + translate)
        } else if (range == 0) {
          storeAtOperand(1, _random.nextInt)
        } else {
          storeAtOperand(1, _random.nextInt(range))
        }
      case 0x111 => // setrandom
        val seed = getOperand(0)
        if (seed == 0) _random.setSeed(seed)
        else _random.setSeed(System.currentTimeMillis)
      case 0x120 => // quit
        state.runState = VMRunStates.Halted
      case 0x121 => // verify
        storeAtOperand(0, state.verify)
      case 0x122 => // restart
        restart
      case 0x123 => // save
        val streamId = getOperand(0)
        val writer = new SaveGameWriter(_glk, streamId, state, _operands(1))
        val result = writer.writeGameFile
        if (result) storeAtOperand(1, 0)
        else storeAtOperand(1, 1)
      case 0x124 => // restore
        val streamId = getOperand(0)
        val loader = new SaveGameLoader(_glk, streamId, state, _originalRam)
        if (loader.loadGame) {
          state.popCallStubThrow(-1)
        } else storeAtOperand(1, 1) // fail for now
      case 0x125 => // saveundo
        _undoSnapshots ::= state.createSnapshot(_operands(0))
        storeAtOperand(0, 0) // Always say SUCCEED
      case 0x126 => // restoreundo
        if (_undoSnapshots != Nil) {
          state.readSnapshot(_undoSnapshots.head, _protectionStart,
                             _protectionLength)
          _undoSnapshots = _undoSnapshots.tail
          state.popCallStubThrow(-1)
        } else {
          storeAtOperand(0, 1) // fail
        }
        logger.info(
          "RESTORED WITH PC: %02x AND FP: %d SP: %d".format(state.pc,
                                                            state.fp,
                                                            state.sp))
      case 0x127 => // protect
        _protectionStart  = getOperand(0)
        _protectionLength = getOperand(1)
      case 0x130 => // glk
        val glkId = getOperand(0)
        val numArgs = getOperand(1)
        val args = new Array[Int](numArgs)
        var i = 0
        while (i < numArgs) {
          args(i) = state.popInt
          i += 1
        }
        val glkResult = _glkDispatch.dispatch(glkId, args)
        //printf("GLK result = #$%02x\n", glkResult)
        storeAtOperand(2, glkResult)
      case 0x140 => // getstringtbl
        storeAtOperand(0, currentDecodingTable)
      case 0x141 => // setstringtbl
        val newDecodingTable = getOperand(0)
        currentDecodingTable = newDecodingTable
        if (newDecodingTable == 0)
          logger.warning("CUSTOM DECODING TABLE SET TO 0 !!!")
      case 0x148 => // getiosys
        storeAtOperand(0, currentIOSystem.id)
        storeAtOperand(1, currentIOSystem.rock)
      case 0x149 => // setiosys
        val iosys = getOperand(0)
        val rock  = getOperand(1)
        currentIOSystem = (iosys: @switch) match {
          case 0  => new NullIOSystem(this, rock)
          case 1  => new FilterIOSystem(this, rock)
          case 2  => new GlkIOSystem(this, _glk, rock)
          case 20 => new ChannelIOSystem(this, rock)
          case _ =>
            throw new UnsupportedOperationException(
              "IO system[%d] not supported".format(iosys))
        }
      case 0x150 => // linearsearch
        val search = new LinearSearch(state, getOperand(0), getOperand(1),
                                      getOperand(2), getOperand(3),
                                      getOperand(4), getOperand(5),
                                      getOperand(6))
        storeAtOperand(7, search.search)
      case 0x151 => // binarysearch
        val search = new BinarySearch(state, getOperand(0), getOperand(1),
                                      getOperand(2), getOperand(3),
                                      getOperand(4), getOperand(5),
                                      getOperand(6))
        val result = search.search
        storeAtOperand(7, result)
      case 0x152 => // linkedsearch
        val search = new LinkedSearch(state, getOperand(0), getOperand(1),
                                      getOperand(2), getOperand(3),
                                      getOperand(4), getOperand(5))
        storeAtOperand(6, search.search)
      case 0x160 => // callf
        doCallf(getOperand(0), _operands(1))
      case 0x161 => // callfi
        doCallf(getOperand(0), _operands(2), getOperand(1))
      case 0x162 => // callfii
        doCallf(getOperand(0), _operands(3), getOperand(1), getOperand(2))
      case 0x163 => // callfiii
        doCallf(getOperand(0), _operands(4), getOperand(1), getOperand(2),
                getOperand(3))
      case 0x170 => // mzero
        state.mzero(getOperand(0), getOperand(1))
      case 0x171 => // mcopy
        state.mcopy(getOperand(0), getOperand(1), getOperand(2))
      case 0x178 => // malloc
        storeAtOperand(1, state.malloc(getOperand(0)))
      case 0x179 => // mfree
        state.mfree(getOperand(0))
      case 0x180  => // accelfunc
        _accelSystem.setFunction(getOperand(0), getOperand(1))
      case 0x181  => // accelparam
        _accelSystem.setParameter(getOperand(0), getOperand(1))
      case 0x190 => // numtof
        val operand1 = getOperand(0).asInstanceOf[Float]
        storeAtOperand(1, java.lang.Float.floatToRawIntBits(operand1))
      case 0x191 => // ftonumz
        storeAtOperand(1, GlulxFloat.ftonumz(getOperand(0)))
      case 0x192 => // ftonumn
        storeAtOperand(1, GlulxFloat.ftonumn(getOperand(0)))
      case 0x198 => // ceil
        storeAtOperand(1, GlulxFloat.ceil(getOperand(0)))
      case 0x199 => // floor
        storeAtOperand(1, GlulxFloat.floor(getOperand(0)))
      case 0x1a0 => // fadd
        storeAtOperand(2, GlulxFloat.fadd(getOperand(0), getOperand(1)))
      case 0x1a1 => // fsub
        storeAtOperand(2, GlulxFloat.fsub(getOperand(0), getOperand(1)))
      case 0x1a2 => // fmul
        storeAtOperand(2, GlulxFloat.fmul(getOperand(0), getOperand(1)))
      case 0x1a3 => // fdiv
        storeAtOperand(2, GlulxFloat.fdiv(getOperand(0), getOperand(1)))
      case 0x1a4 => // fmod
        val operand1 = getOperand(0)
        val operand2 = getOperand(1)
        storeAtOperand(2, GlulxFloat.fmodRemainder(operand1, operand2))
        storeAtOperand(3, GlulxFloat.fmodQuotient(operand1, operand2))
      case 0x1a8 => // sqrt
        storeAtOperand(1, GlulxFloat.sqrt(getOperand(0)))
      case 0x1a9 => // exp
        storeAtOperand(1, GlulxFloat.exp(getOperand(0)))
      case 0x1aa => // log
        storeAtOperand(1, GlulxFloat.log(getOperand(0)))
      case 0x1ab => // pow
        storeAtOperand(2, GlulxFloat.pow(getOperand(0), getOperand(1)))
      case 0x1b0 => // sin
        storeAtOperand(1, GlulxFloat.sin(getOperand(0)))
      case 0x1b1 => // cos
        storeAtOperand(1, GlulxFloat.cos(getOperand(0)))
      case 0x1b2 => // tan
        storeAtOperand(1, GlulxFloat.tan(getOperand(0)))
      case 0x1b3 => // asin
        storeAtOperand(1, GlulxFloat.asin(getOperand(0)))
      case 0x1b4 => // acos
        storeAtOperand(1, GlulxFloat.acos(getOperand(0)))
      case 0x1b5 => // atan
        storeAtOperand(1, GlulxFloat.atan(getOperand(0)))
      case 0x1b6 => // atan2
        storeAtOperand(2, GlulxFloat.atan2(getOperand(0), getOperand(1)))
      case 0x1c0 => // jfeq
        if (GlulxFloat.feq(getOperand(0), getOperand(1), getOperand(2)))
          doBranch(getOperand(3))
      case 0x1c1 => // jfne
        if (GlulxFloat.fne(getOperand(0), getOperand(1), getOperand(2)))
          doBranch(getOperand(3))
      case 0x1c2 => // jflt
        if (GlulxFloat.flt(getOperand(0), getOperand(1)))
          doBranch(getOperand(2))
      case 0x1c3 => // jfle
        if (GlulxFloat.fle(getOperand(0), getOperand(1)))
          doBranch(getOperand(2))
      case 0x1c4 => // jfgt
        if (GlulxFloat.fgt(getOperand(0), getOperand(1)))
          doBranch(getOperand(2))
      case 0x1c5 => // jfge
        if (GlulxFloat.fge(getOperand(0), getOperand(1)))
          doBranch(getOperand(2))
      case 0x1c8 => // jisnan
        if (GlulxFloat.isNaN(getOperand(0))) doBranch(getOperand(1))
      case 0x1c9 => // jisinf
        if (GlulxFloat.isInfinity(getOperand(0))) doBranch(getOperand(1))
      case _ => throw new IllegalArgumentException(
        "unknown opcode number: %02x".format(_opcodeNum))
    }
  }

  // decode instruction at current pc
  def doInstruction {
    val pc = state.pc
    decodeOpcodeNum
    readOperands
/*
    // for debugging
    val builder = new StringBuilder
    builder.append("%04d: $%04x - @%s".format(iterations, pc, Opcodes.name(_opcodeNum)))
    val numOperands = Opcodes.numOperands(_opcodeNum)
    for (i <- 0 until numOperands) {
      builder.append(" %s".format(_operands(i).toString(state)))
    }
    builder.append(" {FP = %d SP = %d} ".format(state.fp, state.sp))
    builder.append(" " + state.stackValuesAsString)
    logger.info(builder.toString)
*/  
    executeInstruction
    iterations += 1
  }

  def fatal(msg: String) {
    _glk.put_java_string(msg)
    state.runState = VMRunStates.Halted
  }
}


