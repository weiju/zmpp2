/*
 * Created on 2011/02/12
 * Copyright (c) 2010-2012, Wei-ju Wu.
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
package org.zmpp.glulx;

import java.util.logging.*;
import org.zmpp.base.*;

// ***************************************************************************
// ****
// **** VM state
// ****
// ***************************************************************************
class Stack {
    public static final int OffsetLocalsPos     = 4;
    public static final int OffsetLocalsFormat  = 8;
}

/**
 * GlulxVMState captures the internal state of the Glulx system:
 * - story memory
 * - RAM access
 * - stack access
 *  - local variables
 *  - functions
 */
public class GlulxVMState implements VMState {
    private static final int SizeLocalDescriptor = 2;
    private static final int ByteType   = 1;
    private static final int ShortType  = 2;
    private static final int IntType    = 4;
  
    private static final int SizeByte   = 1;
    private static final int SizeShort  = 2;
    private static final int SizeInt    = 4;

    private Logger logger = Logger.getLogger("glulx");
    public int pRunState = VMRunStates.Running;
    private byte[] _storyBytes;

    // Memory setup
    private MemoryHeap _memheap;
    private Memory _extMem;
    private int _extEnd;
    private int _extstart; // cached, because accessed frequently

    public BinarySearch binarySearch = new BinarySearch(this);
    public LinearSearch linearSearch = new LinearSearch(this);
    public LinkedSearch linkedSearch = new LinkedSearch(this);
    public StoryHeader header;

    // Stack
    private byte[] _stackArray;
    public int sp;

    // Registers
    public int pc;
    public int fp;

    public void init(byte[] storyBytes) {
        _storyBytes = storyBytes;
        header      = new StoryHeader(storyBytes);
        initStack();
        _extstart   = header.extstart();
        _memheap    = new MemoryHeap(header.endmem());
        pc          = 0;
        fp          = 0;
        pRunState   = VMRunStates.Running;
        setMemsize(header.endmem());
        logger.info(String.format("VM INITIALIZED WITH EXT_START: %d END_MEM: %d",
                                  _extstart, header.endmem()));
    }

    private void initStack() {
        _stackArray = new byte[header.stacksize()];
        sp         = 0;
    }

    /**
     * Yessir, we need to read the stack directly, to save some cycles in Inform7 games.
     */
    public byte[] stackBytes() { return _stackArray; }

    public int runState() { return pRunState; }
    public void setRunState(int state) { this.pRunState = state; }

    public void restart(byte[] originalRam, int protectionStart, int protectionLength) {
        logger.info(String.format("@restart (start: %02x, # bytes: %02x)",
                                  protectionStart, protectionLength));
        logger.info(String.format("HEAP ACTIVE: %b EXT_START: $%02x EXT_END: $%02x",
                                  _memheap.active(), _extstart, _extEnd));
        initStack();
        _memheap   = new MemoryHeap(header.endmem());
        setMemsize(header.endmem());
        pc         = 0;
        fp         = 0;
        pRunState  = VMRunStates.Running;

        // reset bytes in ram
        int ramsize = _extstart - header.ramstart();
        protectedMemRestore(originalRam, 0, header.ramstart(), ramsize,
                            protectionStart, protectionLength);
    }
  
    private void protectedMemRestore(byte[] memarray, int srcOffset,
                                     int destOffset, int numBytes,
                                     int protectionStart,
                                     int protectionLength) {
        if (protectionLength > 0) {
            for (int i = 0; i < numBytes; i++) {                
                int destAddress = destOffset + i;
                if (destAddress < protectionStart ||
                    destAddress >= protectionStart + protectionLength) {
                    _storyBytes[destAddress] = (byte) memarray[i];
                }
            }
        } else {
            //_story.copyBytesFrom(memarray, srcOffset, destOffset, numBytes)
            System.arraycopy(memarray, srcOffset, _storyBytes, destOffset, numBytes);
        }
    }

    private void _setExtendedMem(int size) {
        int currentExtSize = (_extMem == null) ? 0 : _extEnd - _extstart;
        // note: we actually do not need to "shrink" extended memory, we only
        // need to extend it
        if (currentExtSize != size) {
            if (size > currentExtSize) {
                byte[] extbytes = new byte[size];
                if (_extMem != null) {
                    _extMem.copyBytesTo(extbytes, _extstart, currentExtSize);
                }
                _extMem = new DefaultMemory(extbytes, _extstart);
            }
        }
        _extEnd = _extstart + size;
    }

    public boolean heapIsActive() { return _memheap.active(); }
    public int ramSize() { return memsize() - header.ramstart(); }

    private boolean inStoryMem(int addr) { return addr < _extstart; }
    private boolean inExtMem(int addr)   { return addr >= _extstart && addr < _extEnd; }
    private boolean fitsInStoryMem(int addr, int size) {
        return inStoryMem(addr) && inStoryMem(addr + size - 1);
    }

    private boolean fitsOnHeap(int addr, int size) {
        return _memheap.memblockAt(addr) != null;
    }

    // Stack interface
    public boolean stackEmpty() { return sp == 0; }

    public int localsPos() { return getIntInStack(fp + Stack.OffsetLocalsPos); }
    public int frameLen() { return getIntInStack(fp); }

    private String stackToStringFromTo(int start, int end) {
        StringBuilder builder = new StringBuilder();
        builder.append("(Stack [" + start + "-" + end + ")) = [");
        for (int i = start; i < end; i++) {
            if (i > start) builder.append(", ");
            builder.append(String.format("%02x", _stackArray[i]));
        }
        builder.append("]");
        return builder.toString();
    }

    public String stackToStringFrom(int start) { return stackToStringFromTo(start, sp); }

    public byte[] cloneStackValues() {
        byte[] values = new byte[sp];
        System.arraycopy(_stackArray, 0, values, 0, sp);
        return values;
    }

    public void initStackFromByteArray(byte[] array) {
        sp = array.length;
        System.arraycopy(array, 0, _stackArray, 0, sp);
    }

    public void pushByte(int value) {
        _stackArray[sp++] = (byte) value;
    }
    public int topByte() { return _stackArray[sp - 1] & 0xff; }

    public int popByte() {
        if (sp <= fp + frameLen())
            throw new IllegalStateException("POP BYTE - STACK UNDERFLOW !!");
        return (int) _stackArray[--sp] & 0xff;
    }
    public void pushShort(int value) {
        _stackArray[sp]     = (byte) ((value >>> 8) & 0xff);
        _stackArray[sp + 1] = (byte) (value & 0xff);
        sp += 2;
    }
    public int topShort() {
        return ((_stackArray[sp - 2] & 0xff) << 8) | (_stackArray[sp - 1] & 0xff);
    }
    public int popShort() {
        if (sp <= fp + frameLen())
            throw new IllegalStateException("POP SHORT - STACK UNDERFLOW !!");
        sp -= 2;
        return ((_stackArray[sp] & 0xff) << 8) | (_stackArray[sp + 1] & 0xff);
    }
    public void pushInt(int value) {
        _stackArray[sp] = (byte) ((value >>> 24) & 0xff);
        _stackArray[sp + 1] = (byte) ((value >>> 16) & 0xff);
        _stackArray[sp + 2] = (byte) ((value >>> 8) & 0xff);
        _stackArray[sp + 3] = (byte) (value & 0xff);
        sp += 4;
    }

    public int topInt() {
        return ((_stackArray[sp - 4] & 0xff) << 24) | ((_stackArray[sp - 3] & 0xff) << 16) |
            ((_stackArray[sp - 2] & 0xff) << 8) | (_stackArray[sp - 1] & 0xff);
    }
    public int popIntUnchecked() {
        sp -= 4;
        return ((_stackArray[sp] & 0xff) << 24) | ((_stackArray[sp + 1] & 0xff) << 16) |
            ((_stackArray[sp + 2] & 0xff) << 8) | (_stackArray[sp + 3] & 0xff);
    }
    public int popInt() {
        if (sp <= fp + frameLen())
            throw new IllegalStateException("POP INT - STACK UNDERFLOW !!");
        sp -= 4;
        return ((_stackArray[sp] & 0xff) << 24) | ((_stackArray[sp + 1] & 0xff) << 16) |
            ((_stackArray[sp + 2] & 0xff) << 8) | (_stackArray[sp + 3] & 0xff);
    }

    public void setIntInStack(int addr, int value) {
        _stackArray[addr] = (byte) ((value >>> 24) & 0xff);
        _stackArray[addr + 1] = (byte) ((value >>> 16) & 0xff);
        _stackArray[addr + 2] = (byte) ((value >>> 8) & 0xff);
        _stackArray[addr + 3] = (byte) (value & 0xff);
    }
    public int getIntInStack(int addr) {
        return ((_stackArray[addr] & 0xff) << 24) | ((_stackArray[addr + 1] & 0xff) << 16) |
            ((_stackArray[addr + 2] & 0xff) << 8) | (_stackArray[addr + 3] & 0xff);
    }

    public void setShortInStack(int addr, int value) {
        _stackArray[addr] = (byte) ((value >>> 8) & 0xff);
        _stackArray[addr + 1] = (byte) (value & 0xff);
    }

    public int getShortInStack(int addr) {
        return ((_stackArray[addr] & 0xff) << 8) | (_stackArray[addr + 1] & 0xff);
    }
    public void setByteInStack(int addr, int value) {
        _stackArray[addr] = (byte) (value & 0xff);
    }
    public int getByteInStack(int addr) {
        return _stackArray[addr] & 0xff;
    }

    public int numStackValuesInCallFrame() { return (sp - (fp + frameLen())) / 4; }
  
    // special stack functions
    public void stackSwap() {
        if (numStackValuesInCallFrame() < 2)
            throw new IllegalStateException("STACK SWAP - NOT ENOUGH STACK VALUES");
        int val0 = popInt();
        int val1 = popInt();
        pushInt(val0);
        pushInt(val1);
    }
    public int stackPeek(int i) {
        if (numStackValuesInCallFrame() <= i) {
            throw new IllegalStateException("STACK PEEK - NOT ENOUGH STACK VALUES");
        }
        return getIntInStack(sp - (4 * (i + 1)));
    }
    public void stackRoll(int numValues, int numRotatePlaces) {
        if (numRotatePlaces == 0) return;
        int[] tmparr = new int[numValues];
        for (int i = 0; i < numValues; i++) {
            int pos = ((numValues - 1 - i) + numRotatePlaces) % numValues;
            if (pos < 0) pos = numValues + pos;
            tmparr[pos] = popInt();
        }
        for (int i = 0; i < numValues; i++) {
            pushInt(tmparr[i]);
        }
    }
    // **********************************************************************
    // ***** MEMORY INTERFACE
    // **********************************************************************

    public int memByteAt(int addr) {
        if (addr < _extstart) return (_storyBytes[addr] & 0xff); // inStoryMem(), manually inlined
        else if (inExtMem(addr)) return _extMem.byteAt(addr);
        else return _memheap.byteAt(addr);
    }

    public void setMemByteAt(int addr, int value) {
        if (addr < header.ramstart()) {
            logger.warning(String.format("SETTING BYTE VALUE IN ROM %02x = %d !",
                                         addr, value));
        }
        if (addr < _extstart)    _storyBytes[addr] = (byte) (value & 0xff);
        else if (inExtMem(addr)) _extMem.setByteAt(addr, value);
        else                     _memheap.setByteAt(addr, value);
    }

    public int memShortAt(int addr) {
        if (addr < _extstart) {
            return ((_storyBytes[addr] & 0xff) << 8) | (_storyBytes[addr + 1] & 0xff);
        } else if (inExtMem(addr)) return _extMem.shortAt(addr);
        else return _memheap.shortAt(addr);
    }

    public void setMemShortAt(int addr, int value) {
        if (addr < header.ramstart()) {
            logger.warning(String.format("SETTING SHORT VALUE IN ROM %02x = %d !",
                                         addr, value));
        }
        if (addr < _extstart) {
            _storyBytes[addr]     = (byte) ((value >>> 8) & 0xff);
            _storyBytes[addr + 1] = (byte) (value & 0xff);
        } else if (inExtMem(addr))   _extMem.setShortAt(addr, value);
        else                       _memheap.setShortAt(addr, value);
    }

    public int memIntAt(int addr) {
        if (addr < _extstart) {
            return ((_storyBytes[addr] & 0xff) << 24) | ((_storyBytes[addr + 1] & 0xff) << 16) |
                ((_storyBytes[addr + 2] & 0xff) << 8) | (_storyBytes[addr + 3] & 0xff);
        } else if (inExtMem(addr)) return _extMem.intAt(addr);
        else return _memheap.intAt(addr);
    }

    public void setMemIntAt(int addr, int value) {
        if (addr < header.ramstart()) {
            logger.warning(String.format("SETTING INT VALUE IN ROM %02x = %d !",
                                         addr, value));
        }
        if (addr < _extstart) {
            _storyBytes[addr]     = (byte) ((value >>> 24) & 0xff);
            _storyBytes[addr + 1] = (byte) ((value >>> 16) & 0xff);
            _storyBytes[addr + 2] = (byte) ((value >>> 8) & 0xff);
            _storyBytes[addr + 3] = (byte) (value & 0xff);
        } else if (inExtMem(addr)) _extMem.setIntAt(addr, value);
        else                       _memheap.setIntAt(addr, value);
    }

    public int ramByteAt(int address) { return memByteAt(header.ramstart() + address); }
    public void setRamByteAt(int address, int value) {
        setMemByteAt(header.ramstart() + address, value);
    }
    public int ramShortAt(int address) { return memShortAt(header.ramstart() + address); }
    public void setRamShortAt(int address, int value) {
        setMemShortAt(header.ramstart() + address, value);
    }
    public int ramIntAt(int address) { return memIntAt(header.ramstart() + address); }
    public void setRamIntAt(int address, int value) {
        setMemIntAt(header.ramstart() + address, value);
    }

    public int malloc(int size) { return _memheap.allocate(size); }
    public void mfree(int addr) { _memheap.free(addr); }
    public void mcopy(int numBytes, int srcAddr, int destAddr) {
        if (fitsInStoryMem(srcAddr, numBytes) &&
            fitsInStoryMem(destAddr, numBytes)) {
            //_story.copyBytesTo(destAddr, srcAddr, numBytes);
            System.arraycopy(_storyBytes, srcAddr, _storyBytes, destAddr, numBytes);
        } else if (fitsOnHeap(srcAddr, numBytes) && fitsOnHeap(destAddr, numBytes)) {
            _memheap.copyBytesTo(destAddr, srcAddr, numBytes);
        } else {
            for (int i = 0; i < numBytes; i++) {
                setMemByteAt(destAddr + i, memByteAt(srcAddr + i));
            }
        }
    }

    public void mzero(int numBytes, int addr) {
        for (int i = 0; i < numBytes; i++) {
            setMemByteAt(addr + i, 0);
        }
    }
    public int memsize() { return (_memheap.active()) ? _memheap.maxAddress() : _extEnd; }
    public void setMemsize(int newSize) { _setExtendedMem(newSize - _extstart); }
    public int heapStart() { return (_memheap.active()) ? _extEnd : 0; }

    // **********************************************************************
    // ***** FUNCTION CALLS
    // **********************************************************************

    // Pushes a call stub, given an operand
    public void pushCallStub(Operand storeLocation) {
        pushCallStub(DestTypes.fromAddressMode(storeLocation.addressMode),
                     storeLocation.value, pc, fp);
    }
    public void pushCallStub(int destType, int destAddr) {
        pushCallStub(destType, destAddr, pc, fp);
    }
    // generic call stub pushing, can take other values for pc and fp
    public void pushCallStub(int destType, int destAddr, int pcVal, int fpVal) {
        pushInt(destType);
        pushInt(destAddr);
        pushInt(pcVal);
        pushInt(fpVal);
    }
  
    public void popCallStubThrow(int retval) {
        fp           = popInt();
        pc           = popInt();
        int destAddr = popInt();
        int destType = popInt();
        storeResult(destType, destAddr, retval);
    }

    private void setValueInStack(int index, int vartype, int value) {
        switch (vartype) {
        case 1: // Types.ByteType
            setByteInStack(index, value);
            break;
        case 2: // Types.ShortType
            setShortInStack(index, value);
            break;
        case 4: // Types.IntType
            setIntInStack(index, value);
            break;
        default:
            throw new IllegalStateException("unknown local type: " + vartype);
        }
    }

    public void storeResult(int destType, int destAddress, int value) {
        switch (destType) {
        case 0: // DestTypes.DoNotStore, do nothing
            break;
        case 1: // DestTypes.Memory
            setMemIntAt(destAddress, value);
            break;
        case 2: // DestTypes.LocalVariable
            setLocalAtAddress(destAddress, value);
            break;
        case 3: // DestTypes.Stack
            pushInt(value);
            break;
        case 4: // DestTypes.Ram
            setRamIntAt(destAddress, value);
            break;
        default:
            throw new IllegalArgumentException(String.format("unsupported dest type for store: ",
                                                             destType));
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
    private int alignAddress(int address, int datatype) {
        // set to multiple of 4 for ltype == 4 and to even for ltype == 2
        if (datatype == IntType && ((address & 0x03) != 0)) {
            return address + (SizeInt - (address & 0x03));
        } else if (datatype == ShortType && ((address & 0x01) != 0)) {
            return address + SizeByte;
        } else {
            return address;
        }
    }

    // Stores an int at the specified address within the current frame's local
    // space. Note that we do not check the type, simply set the value without
    // a check with type Int. A couple of games seem to rely on the interpreter
    // letting them to write to locals that do not exist.
    public void setLocalAtAddress(int destAddr, int value) {
        setIntInStack(fp + localsPos() + destAddr, value);
    }

    // Analogous to setLocalAtAddress(), this returns an int-sized value
    // and does not check the format, as the Glulx specification requests.
    public int getLocalAtAddress(int localAddr) {
        // * ugly optimization * inlined the following:
        // int lpos = localsPos();
        // return getIntInStack(fp + lpos + localAddr);
        int lposAddr = fp + Stack.OffsetLocalsPos;
        int lpos = ((_stackArray[lposAddr] & 0xff) << 24) | ((_stackArray[lposAddr + 1] & 0xff) << 16) |
            ((_stackArray[lposAddr + 2] & 0xff) << 8) | (_stackArray[lposAddr + 3] & 0xff);
        int addr = fp + lpos + localAddr;
        return ((_stackArray[addr] & 0xff) << 24) | ((_stackArray[addr + 1] & 0xff) << 16) |
            ((_stackArray[addr + 2] & 0xff) << 8) | (_stackArray[addr + 3] & 0xff);
    }
  
    // For copyb/copys
    public int getLocalByteAtAddress(int localAddr) {
        return getByteInStack(fp + localsPos() + localAddr);
    }
    public int getLocalShortAtAddress(int localAddr) {
        return getShortInStack(fp + localsPos() + localAddr);
    }
    public void setLocalByteAtAddress(int destAddr, int value) {
        setByteInStack(fp + localsPos() + destAddr, value);
    }
    public void setLocalShortAtAddress(int destAddr, int value) {
        setShortInStack(fp + localsPos() + destAddr, value);
    }

    // ***********************************************************************
    // ***** Access local variables through variable numbers
    // *************************************************************
    private int localFrameIndex(int localNum) {
        int descriptorPos    = fp + Stack.OffsetLocalsFormat;
        int currentLocalPos  = localsPos();
        int localRangeStart    = 0;
        boolean hasMoreDescriptors = true;

        while (hasMoreDescriptors) {
            int ltype    = getByteInStack(descriptorPos);
            int nlocals  = getByteInStack(descriptorPos + 1);
            // needs alignment of localPos ?
            descriptorPos = alignAddress(descriptorPos, ltype);
            // the variable is within the current range, determine which one
            // and returns its address
            if (localRangeStart + nlocals > localNum) {
                return currentLocalPos + (localNum - localRangeStart) * ltype;
            }
            // adjust variables
            hasMoreDescriptors = ltype != 0 && nlocals != 0;
            currentLocalPos    += ltype * nlocals; // advance to next variable block
            localRangeStart    += nlocals;         // advance range index
            // advance to next descriptor pair
            descriptorPos      += SizeLocalDescriptor;
        }
        throw new IllegalStateException("unknown local variable: " + localNum);
    }

    private int localType(int localNum) {
        int descriptorPos = fp + Stack.OffsetLocalsFormat;
        int localRangeStart    = 0;
        boolean hasMoreDescriptors = true;

        while (hasMoreDescriptors) {
            int ltype    = getByteInStack(descriptorPos);
            int nlocals  = getByteInStack(descriptorPos + 1);
            if (localRangeStart + nlocals > localNum) return ltype;
            hasMoreDescriptors = ltype != 0 && nlocals != 0;

            // advance to next descriptor pair
            descriptorPos   += SizeLocalDescriptor;
            localRangeStart += nlocals;
        }
        return 0;
    }

    public void setLocal(int localNum, int value) {
        int ltype  = localType(localNum);
        if (ltype != 0) {
            int lindex = localFrameIndex(localNum);
            // Note: Only set a local if it exists !!!
            setValueInStack(fp + lindex, ltype, value);
        }
    }

    // do not implement if for now  
    public int verify() { return 0; }
        /*
  override def toString = {
    val builder = new StringBuilder
    builder.append("pc = $%02x stackframe = $%02x\n".format(_pc, _fp))
    builder.append(stackToStringFrom(_fp))
    builder.toString
  }
  
  def stackValuesAsString = {
    val builder = new StringBuilder
    val stackStart = _fp + frameLen
    val numElems = numStackValuesInCallFrame
    builder.append("[")
    var i = 0
    while (i < numElems) {
      if (i > 0) builder.append(", ")
      builder.append("#$%02x".format(getIntInStack(stackStart + (i * 4))))
      i += 1
    }
    builder.append("]")
    builder.toString
  }
    */
  
    // Reading data at the PC
    public int nextShort() {
        pc += 2;
        return memShortAt(pc - 2);
    }
    public int nextInt() {
        pc += 4;
        return memIntAt(pc - 4);
    }

    // ***********************************************************************
    // ***** State Serialization
    // *************************************************************
    public void readSnapshot(Snapshot snapshot, int protectionStart,
                             int protectionLength) {
        int ramsize = _extstart - header.ramstart();
        protectedMemRestore(snapshot.ram, 0, header.ramstart(), ramsize,
                            protectionStart, protectionLength);
        initStackFromByteArray(snapshot.stack);    
        // TODO: Extmem and Heap
    }
  
    public Snapshot createSnapshot(Operand storeLocation) {
        byte[] ram = cloneRam();
        logger.info(String.format("CREATE_SNAPSHOT, PC = $%02x FP = %d SP: %d",
                                  pc, fp, sp));
        pushCallStub(storeLocation);
        byte[] stackValues = cloneStackValues();
        byte[] extmem = null;

        // TODO: extmem and heap
        return new Snapshot(ram, stackValues, extmem);
    }


    public byte[] cloneRam() {
        int ramsize = _extstart - header.ramstart();
        logger.info(String.format("Copying %d Bytes of RAM to preserve initial data", ramsize));
        byte[] ram = new byte[ramsize];
        //_story.copyBytesTo(ram, header.ramstart(), ramsize)
        System.arraycopy(_storyBytes, header.ramstart(), ram, 0, ramsize);
        return ram;
    }
}
