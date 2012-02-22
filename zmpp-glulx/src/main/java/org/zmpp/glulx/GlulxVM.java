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

import org.zmpp.base.*;
import org.zmpp.iff.*;
import org.zmpp.glk.*;
import org.zmpp.glk.events.*;
import java.util.*;
import java.util.logging.*;

/****************************************************************************
 * VM main class. If you think this is ugly, it probably is.
 * Due to heavy optimization, this class is mainly a huge execution
 * loop with lots of redundant code due to optimizations like manual
 * inlining.
 * The Java Hotspot engine can do lots of these optimizations by itself,
 * but not Android's Dalvik VM.
 */
final public class GlulxVM implements VMState {

    // ************************************************************
    // * STATIC MEMBERS
    // ************************************************************

    private static final int MaxLocalDescriptors = 255;
    private static final int MaxOperands         = 10;
    private static final int MaxArguments        = 20;
    private static final int SizeLocalDescriptor = Types.SizeByte * 2;

    public static final  int OffsetLocalsPos     = 4;
    public static final  int OffsetLocalsFormat  = 8;

    private static final int ByteType   = 1;
    private static final int ShortType  = 2;
    private static final int IntType    = 4;
  
    private static final int SizeByte   = 1;
    private static final int SizeShort  = 2;
    private static final int SizeInt    = 4;

    // lookup table for number of operands
    public static final byte[] NumOperands = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00-0x0f
        3, 3, 3, 3, 3, 2, 0, 0, 3, 3, 3, 2, 3, 3, 3, 0, // 0x10-0x1f
        1, 0, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, // 0x20-0x2f
        3, 1, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x30-0x3f
        2, 2, 2, 0, 2, 2, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, // 0x40-0x4f
        1, 2, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x50-0x5f
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x60-0x6f
        1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x70-0x7f
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x80-0x8f
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x90-0x9f
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xa0-0xaf
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xb0-0xbf
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xc0-0xcf
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xd0-0xdf
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xe0-0xef
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xf0-0xff
        3, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x100-0x10f
        2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x110-0x11f
        0, 1, 0, 2, 2, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, // 0x120-0x12f
        3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x130-0x13f
        1, 1, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, // 0x140-0x14f
        8, 8, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x150-0x15f
        2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x160-0x16f
        2, 3, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, // 0x170-0x17f
        2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x180-0x18f
        2, 2, 2, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, // 0x190-0x19f
        3, 3, 3, 3, 4, 0, 0, 0, 2, 2, 2, 3, 0, 0, 0, 0, // 0x1a0-0x1af
        2, 2, 2, 2, 2, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x1b0-0x1bf
        4, 4, 3, 3, 3, 3, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0 // 0x1c0-0x1cf
    };

    private static Logger logger = Logger.getLogger("glulx");
    private static int iterations = 1;

    // *******************************************************************************
    // State is public for subsystems to access. This is the only data
    // that is needed to be serializaed between turns. All other data
    // is scratch data which is only valid during one turn.
    // - story memory
    // - RAM access
    // - stack access
    // - local variables
    // - functions
    // This once was in a separate file, but no more. A lot of Inform 7 games are
    // very, very slow.

    public int pRunState = VMRunStates.Running;
    public byte[] storyBytes;

    // Memory setup
    public MemoryHeap memheap;
    public Memory extMem;
    private int _extEnd;

    public BinarySearch binarySearch = new BinarySearch(this);
    public LinearSearch linearSearch = new LinearSearch(this);
    public LinkedSearch linkedSearch = new LinkedSearch(this);

    // header information
    public int version;
    public int extstart;
    public int ramstart;
    public int endmem;
    public int stacksize;
    public int startfunc;
    public int checksum;

    // Stack
    private byte[] _stackArray;
    public int sp;

    // Registers
    public int pc;
    public int fp;

    // *******************************************************************************
    //
    // VM Execution state
    // 
    // This does not need to be serialized or saved
    //
    // *******************************************************************************
    // Cached currrent frame state. This is used in setting up the current
    // function and is only supposed to be used at that time. When returning
    // from a function, the descriptors are not restored, so do not rely on them !
    // Note: This is mainly an optimization to avoid reading the state over and
    // over again, we might fall back to just reading from memory and pass the
    // state.
    private LocalDescriptor[] _localDescriptors = new LocalDescriptor[MaxLocalDescriptors];
  
    // quick and easy access to the current instruction's values, so we
    // do not need to read them over and over
    private Operand[] _operands = new Operand[MaxOperands];
    private int[] _operandValues = new int[MaxOperands];
    private int _opcodeNum;
    private int _opcodeNumSize;

    // function arguments - we avoid creating them over and over
    // we are using these when we setup a normal function call and
    // we also use them in accelerated functions
    private int[] _arguments    = new int[MaxArguments];
    private int _numArguments;

    // *******************************************************************************
    // SUB SYSTEMS AND HELPERS
    // *******************************************************************************
  
    // VM state
    public  Glk glk;
    private GlkDispatch _glkDispatch;
    private Random _random = new Random();

    private List<Snapshot> _undoSnapshots = new ArrayList<Snapshot>();
    private AccelSystem _accelSystem = new AccelSystem(this);

    // The original state of writable memory after loading
    private byte[] _originalRam;
    private int _protectionStart;
    private int _protectionLength;

    // IO Systems
    public EventManager eventManager() { return glk.eventManager(); }
    public BlorbData blorbData;
    public int currentDecodingTable;
    private IOSystem currentIOSystem = new NullIOSystem(this, 0);

    // *******************************************************************************
    // METHOD AREA
    // *******************************************************************************
    
    public GlulxVM() {
        // initialization
        for (int i = 0; i < MaxLocalDescriptors; i++) {
            _localDescriptors[i] = new LocalDescriptor();
        }
        for (int i = 0; i < MaxOperands; i++) {
            _operands[i] = new Operand();
        }
    }

    public void init(byte[] storyBytes, BlorbData aBlorbData) {
        blorbData = aBlorbData;
        glk = new Glk(new EventManager(this));
        _glkDispatch = new GlkDispatch(this, glk);
        initState(storyBytes);
        _accelSystem.glk = glk;
        if (_originalRam == null) _originalRam = cloneRam();
        prepareCall(startfunc, null);
    }

    public void initState(byte[] storyBytes) {
        this.storyBytes = storyBytes;
        version   = storyIntAt(4);
        ramstart  = storyIntAt(8);
        extstart  = storyIntAt(12);
        endmem    = storyIntAt(16);
        stacksize = storyIntAt(20);
        startfunc = storyIntAt(24);
        currentDecodingTable = storyIntAt(28);
        checksum = storyIntAt(32);

        // init stack
        _stackArray = new byte[stacksize];
        sp         = 0;

        memheap    = new MemoryHeap(endmem);
        pc          = 0;
        fp          = 0;
        pRunState   = VMRunStates.Running;
        setMemsize(endmem);
        logger.info(String.format("VM INITIALIZED WITH EXT_START: %d END_MEM: %d",
                                  extstart, endmem));
    }

    private void restart() {
        restartState(_originalRam, _protectionStart, _protectionLength);
        // copy reference to vm so we can directly access the stack memory (...)
        //_protectionStart  = 0
        //_protectionLength = 0
        //_undoSnapshots = Nil
        prepareCall(startfunc, null);
    }

    private void restartState(byte[] originalRam, int protectionStart, int protectionLength) {
        logger.info(String.format("@restart (start: %02x, # bytes: %02x)",
                                  protectionStart, protectionLength));
        logger.info(String.format("HEAP ACTIVE: %b EXT_START: $%02x EXT_END: $%02x",
                                  memheap.active(), extstart, _extEnd));

        memheap   = new MemoryHeap(endmem);
        setMemsize(endmem);
        pc         = 0;
        fp         = 0;
        sp         = 0;
        pRunState  = VMRunStates.Running;

        // reset bytes in ram
        int ramsize = extstart - ramstart;
        protectedMemRestore(originalRam, 0, ramstart, ramsize,
                            protectionStart, protectionLength);
    }

    private void printState() { System.out.println(toString()); }

    private int readLocalDescriptors(int addr) {
        int currentAddr = addr;
        int descIndex = 0;
        boolean hasMoreDescriptors = true;

        while (hasMoreDescriptors) {
            _localDescriptors[descIndex].localType  = memByteAt(currentAddr);
            _localDescriptors[descIndex].localCount = memByteAt(currentAddr + 1);

            hasMoreDescriptors = !(_localDescriptors[descIndex].localType == 0 &&
                                   _localDescriptors[descIndex].localCount == 0);
            descIndex++;
            currentAddr += SizeLocalDescriptor;
        }
        // include the terminating pair in the count
        return descIndex;
    }

    private int setLocalDescriptorsToCallFrame(int numDescriptors) {
        LocalDescriptor descriptor = null;
        int sp = this.sp;
        for (int i = 0; i < numDescriptors; i++) {
            descriptor = _localDescriptors[i];
            //state.pushByte(descriptor.localType);
            //state.pushByte(descriptor.localCount);
            _stackArray[sp++] = (byte) descriptor.localType;
            _stackArray[sp++] = (byte) descriptor.localCount;
        }
        this.sp = sp; // re-adjust stackpointer

        // Ensure a size dividable by 4 (the size of an int)
        int localDescriptorSize = numDescriptors * Types.SizeShort;
        if ((localDescriptorSize % Types.SizeInt) != 0) {
            pushShort(0);
            localDescriptorSize += Types.SizeShort;
        }
        return localDescriptorSize;
    }

    // returns the size of the locals sections
    private int setLocalsToCallFrame(int numDescriptors) {
        int localSectionSize = 0;
        // we subtract 1 from numDescriptors, because we do not include the
        // terminator
        for (int i = 0; i < (numDescriptors - 1); i++) {
            int numlocals = _localDescriptors[i].localCount;
            int ltype     = _localDescriptors[i].localType;
            if (!Types.isValidType(ltype)) {
                throw new IllegalArgumentException("unknown local type: " + ltype);
            }
            // Padding: For short, pad to even address, for int, pad to multiple
            // of 4
            int numPadBytes = 0;
            if (ltype == Types.ShortType && ((this.sp & 0x01) == 1)) {
                // WAS: pushByte(0);
                _stackArray[this.sp++] = 0;
                numPadBytes = 1;
            } else if (ltype == Types.IntType && ((this.sp & 0x03) != 0)) {
                numPadBytes = Types.SizeInt - (this.sp & 0x03);
                for (int j = 0; j < numPadBytes; j++) {
                    // WAS: pushByte(0);
                    _stackArray[this.sp++] = 0;
                }
            }
            // push numlocals locals of size ltype on the stack, we do this
            // by incrementing the stackpointer, which does not do any initialization
            // to the variables
            int blocksize = numlocals * ltype;
            for (int j = 0; j < blocksize; j++) {
                // WAS: pushByte(0);
                _stackArray[this.sp++] = 0;
            }
            localSectionSize += blocksize + numPadBytes;
        }
        return localSectionSize;
    }

    // Used by both signed and unsigned instructions.
    private int getOperand(int pos) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: return 0; // ConstZero
        case 1:
            //return Types.signExtend8(operand.value);  // ConstByte
            return ((operand.value & 0x80) == 0x80) ? operand.value | 0xffffff00 : operand.value & 0xff;
        case 2: return Types.signExtend16(operand.value); // ConstShort
        case 3: return operand.value;                     // ConstInt
        case 5: case 6: case 7:    // Address00_FF, Address0000_FFFF, AddressAny
            return memIntAt(operand.value);
        case 8: return popInt();                          // Stack
        case 9: case 10: case 11:  // Local00_FF, Local0000_FFFF, LocalAny
            return getLocalAtAddress(operand.value);
        case 13: case 14: case 15: // Ram00_FF, Ram0000_FFFF, RamAny
            return ramIntAt(operand.value);
        default:
            throw new IllegalStateException("unsupported operand type: " +
                                            operand.addressMode);
        }
    }

    // Get a bunch of consecutive operands. This is getOperand(), to be used when
    // there are more than one getOperand() calls. This averages out the ridiculous
    // amount of getOperand() calls when there are a couple million instructions
    // per turn
    private void getOperandValues(int numOperands) {
        Operand operand = null;
        for (int pos = 0; pos < numOperands; pos++) {
            operand = _operands[pos];
            switch (operand.addressMode) {
            case 0: _operandValues[pos] = 0; break; // ConstZero
            case 1:
                //return Types.signExtend8(operand.value);  // ConstByte
                _operandValues[pos] = ((operand.value & 0x80) == 0x80) ?
                    operand.value | 0xffffff00 : operand.value & 0xff;
                break;
            case 2: _operandValues[pos] = Types.signExtend16(operand.value); break; // ConstShort
            case 3: _operandValues[pos] = operand.value; break;                     // ConstInt
            case 5: case 6: case 7:    // Address00_FF/Address0000_FFFF/AddressAny
                _operandValues[pos] = memIntAt(operand.value); break;
            case 8: _operandValues[pos] = popInt(); break;                          // Stack
            case 9: case 10: case 11:  // Local00_FF/Local0000_FFFF/LocalAny
                _operandValues[pos] = getLocalAtAddress(operand.value); break;
            case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
                _operandValues[pos] = ramIntAt(operand.value); break;
            default:
                throw new IllegalStateException("unsupported operand type: " +
                                                operand.addressMode);
            }
        }
    }

    // only for copyb/copys
    // Only used by copyb.
    private int getOperand8(int pos) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: return 0; // ConstZero
        case 1:
            //return Types.signExtend8(operand.value);  // ConstByte
            return ((operand.value & 0x80) == 0x80) ? operand.value | 0xffffff00 : operand.value & 0xff;
        case 2: return Types.signExtend16(operand.value); // ConstShort
        case 3: return operand.value;                     // ConstInt
        case 5: case 6: case 7:    // Address00_FF/Address0000_FFFF/AddressAny
            return memByteAt(operand.value);
        case 8: return popInt();                    // Stack
        case 9: case 10: case 11:  // Local00_FF/Local0000_FFFF/LocalAny 
            return getLocalByteAtAddress(operand.value);
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            return ramByteAt(operand.value);
        default:
          throw new IllegalStateException("unsupported operand type: " +
                                          operand.addressMode);
        }
    }

    // Only used by copys.
    private int getOperand16(int pos) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: return 0; // ConstZero
        case 1:
            // return Types.signExtend8(operand.value); // ConstByte
            return ((operand.value & 0x80) == 0x80) ? operand.value | 0xffffff00 : operand.value & 0xff;
        case 2: return Types.signExtend16(operand.value); // ConstShort
        case 3: return operand.value; // ConstInt
        case 5: case 6: case 7: // Address00_FF/Address_0000_FFFF/AddressAny
            return memShortAt(operand.value);
        case 8: return popInt(); // Stack
        case 9: case 10: case 11: // Local00_FF/Local0000_FFFF/LocalAny
            return getLocalShortAtAddress(operand.value);
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            return ramShortAt(operand.value);
        default:
            throw new IllegalStateException("unsupported operand type: " +
                                            operand.addressMode);
        }
    }

    // ***********************************************************************
    // ***** Storing
    // *********************************
    private void storeAtOperand(int pos, int value) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: // ConstZero, throw result away
            break;
        case 5: case 6: case 7: // Address00_FF, Address0000_FFFF, AddressAny
            setMemIntAt(operand.value, value);
            break;
        case 8:
            pushInt(value); // Stack
            break;
        case 9: case 10: case 11: // Local00_FF, Local0000_FFFF, LocalAny
            setLocalAtAddress(operand.value, value);
            break;
        case 13: case 14: case 15: // Ram00_FF, Ram0000_FFFF, RamAny
            setRamIntAt(operand.value, value);
            break;
        default:
            throw new IllegalArgumentException("unsupported address mode for store: " +
                                               operand.addressMode);
        }
    }

    // Only used by copyb.
    private void storeAtOperand8(int pos, int value) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: break; // ConstZero, throw result away
        case 5: case 6: case 7: // Address00_FF/Address0000_FFFF/AddressAny
            setMemByteAt(operand.value, value); break;
        case 8: pushInt(value); break; // Stack
        case 9: case 10: case 11: // Local00_FF/Local0000_FFFF/LocalAny
            setLocalByteAtAddress(operand.value, value); break;
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            setRamByteAt(operand.value, value); break;
        default:
            throw new IllegalArgumentException(String.format("unsupported address mode for store: ",
                                                             operand.addressMode));
        }
    }

    // Only used by copys, 16-bit values
    private void storeAtOperand16(int pos, int value) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: break; // ConstZero, throw result away
        case 5: case 6: case 7: // Address00_FF/Address0000_FFFF/AddressAny
            setMemShortAt(operand.value, value); break;
        case 8: pushInt(value); break; // Stack
        case 9: case 10: case 11: // Local00_FF/Local0000_FFFF/LocalAny
            setLocalShortAtAddress(operand.value, value); break;
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            setRamShortAt(operand.value, value); break;
        default:
          throw new IllegalArgumentException("unsupported address mode for store: " +
                                             operand.addressMode);
        }
    }

    // ***********************************************************************
    // ***** Functions
    // *********************************
    // decodes specified function: initializes the call frame
    // this does *NOT* push a call stub
    private void callFunction(int funaddr) {
        // create call frame (might have to be pushed to the state class)
        pushInt(0); // call frame size
        pushInt(0); // locals position

        int funtype = memByteAt(funaddr);
        int numDescriptors = readLocalDescriptors(funaddr + 1);
        int localDescriptorSize = setLocalDescriptorsToCallFrame(numDescriptors);

        // now that we know the size of the local descriptors section, we set
        // the position of locals
        setIntInStack(this.fp + OffsetLocalsPos,
                      localDescriptorSize + 8);
        int localSectionSize = setLocalsToCallFrame(numDescriptors);

        if (funtype == 0xc0) { // stack-arg type
            // push arguments backwards, then the number of arguments
            for (int i = 0; i < _numArguments; i++) {
                pushInt(_arguments[_numArguments - i - 1]);
            }
            pushInt(_numArguments);
        } else if (funtype == 0xc1) { // local-arg type
            // Copy arguments on the stack backwards to the locals
            for (int i = 0; i < _numArguments; i++) {
                setLocal(i, _arguments[i]);
            }
        } else {
            throw new IllegalArgumentException(String.format("unsupported function type: %02x",
                                                             funtype));
        }

        // set frame len
        setIntInStack(fp, localsPos() + localSectionSize);
        // jump to the code
        pc = funaddr + 1 + SizeLocalDescriptor * numDescriptors;
    }
    // normal function call, called by the VM itself
    private void prepareCall(int funaddr, Operand storeLocation) {
        if (storeLocation != null) {
            pushCallStub(storeLocation);
            fp = sp;
        }
        if (_accelSystem.isAccelerated(funaddr)) {
            // 4 dummy ints on the stack to trick the check
            pushInt(0);
            pushInt(0);
            pushInt(0);
            pushInt(0);
            _accelSystem.call(funaddr, _arguments, _numArguments);
        } else {
            callFunction(funaddr);
        }
    }
  
    // called by @tailcall
    private void tailCall(int funaddr, int numArgs) {
        _numArguments = numArgs;
        for (int i = 0; i < _numArguments; i++) {
            _arguments[i] = popInt();
        }
        sp = fp;
        prepareCall(funaddr, null);
    }

    // Implements @callf, @callfi, @callfii, @callfiii.
    // Originally these 4 functions were a single one, with a vararg parameter.
    // Replaced with direct implementations to avoid autoboxing and the resulting
    // expensive GC.
    //
    private void doCallf0(int funaddr, Operand storeLocation) {
        _numArguments = 0;
        prepareCall(funaddr, storeLocation);
    }

    private void doCallf1(int funaddr, Operand storeLocation, int arg) {
        _arguments[0] = arg;
        _numArguments = 1;
        prepareCall(funaddr, storeLocation);
    }
    private void doCallf2(int funaddr, Operand storeLocation, int arg0, int arg1) {
        _arguments[0] = arg0;
        _arguments[1] = arg1;
        _numArguments = 2;
        prepareCall(funaddr, storeLocation);
    }
    private void doCallf3(int funaddr, Operand storeLocation, int arg0, int arg1, int arg2) {
        _arguments[0] = arg0;
        _arguments[1] = arg1;
        _arguments[2] = arg2;
        _numArguments = 3;
        prepareCall(funaddr, storeLocation);
    }

    // Perform a call given an int array as arguments, this method is called
    // by the I/O system.
   public void callWithArgs(int destType, int destAddr, int pcVal, int fpVal,
                            int funaddr, int[] args) {
       for (int i = 0; i < args.length; i++) {
           _arguments[i] = args[i];
       }
       _numArguments = args.length;
       pushCallStub(destType, destAddr, pcVal, fpVal);
       fp = sp;
       callFunction(funaddr);
   }

    public void callWithArgs(int destType, int destAddr, int pcVal, int fpVal,
                             int funaddr, int arg0) {
        _arguments[0] = arg0;
        _numArguments = 1;
        pushCallStub(destType, destAddr, pcVal, fpVal);
        fp = sp;
        callFunction(funaddr);
    }

    public void callWithArgsNoCallStub(int funaddr, int[] args) {
        for (int i = 0; i < args.length; i++) {
            _arguments[i] = args[i];
        }
        _numArguments = args.length;
        fp = sp;
        callFunction(funaddr);
    }
    public void callWithArgsNoCallStub(int funaddr) {
        _numArguments = 0;
        fp = sp;
        callFunction(funaddr);
    }

    public void callWithArgsNoCallStub(int funaddr, int arg0) {
        _arguments[0] = arg0;
        _numArguments = 1;
        fp = sp;
        callFunction(funaddr);
    }

    // Returns from a function
    public void popCallStub(int retval) {
        if (sp < fp + 12) {
            throw new IllegalStateException("popCallStub(), stack is too small !!");
        }
        sp = fp;
        if (sp == 0) {
            // return from entry function -> Quit
            pRunState = VMRunStates.Halted;
        } else {
            // we can't use GlulxVM's popInt(), because it performs checks on
            // the call frame, which is exactly what we manipulate here
            int fpValue  = popIntUnchecked();
            int pcValue  = popIntUnchecked();
            int destAddr = popIntUnchecked();
            int destType = popIntUnchecked();
            if (DestTypes.isStringDestType(destType)) {
                handleStringCallStub(destType, destAddr, pcValue, fpValue);
            } else { // regular behaviour
                fp = fpValue;
                pc = pcValue;
                storeResult(destType, destAddr, retval);
            }
        }
    }

    private void handleStringCallStub(int destType, int destAddr, int pcValue,
                                      int fpValue) {
        switch (destType) {
        case 10: // DestTypes.ResumePrintCompressed
            currentIOSystem.streamStr(StreamStrState.resumeAt(pcValue, destAddr));
            break;
        case 12: // DestTypes.ResumePrintDecimal
            currentIOSystem.streamNum(pcValue, destAddr);
            break;
        case 13: // DestTypes.ResumePrintCString
            currentIOSystem.streamStr(StreamStrState.resumeCStringAt(pcValue));
            break;
        case 14: // DestTypes.ResumePrintUnicode
            currentIOSystem.streamStr(StreamStrState.resumeUniStringAt(pcValue));
            break;
        default: // do nothing
            throw new UnsupportedOperationException(String.format("Encountered call stub type: %d",
                                                                  destType));
        }
    }

    // ***********************************************************************
    // ***** Strings
    // *********************************
    // ***********************************************************************
    // ***** Branches
    // *********************************
    private void doBranch(int offset) {
        if (offset == 0 || offset == 1) popCallStub(offset);
        else pc += offset - 2;
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

    // decode instruction at current pc
    public void executeTurn() {
        long startTime = System.currentTimeMillis();
        while (pRunState == VMRunStates.Running) {
            // decode opcode number
            // look at the two highest bits: 11 means 4 bytes, 10 means 2 bytes
            // else one byte
            // ******************************************
            int b0 = storyBytes[pc] & 0xff;
            // ******************************************

            int bitpattern = b0 & 0xc0;

            if (bitpattern == 0xc0) {
                _opcodeNum = memIntAt(pc) - 0xc0000000;
                _opcodeNumSize = Types.SizeInt;
            } else if (bitpattern == 0x80) {
                _opcodeNum = (memShortAt(pc) & 0xffff) - 0x8000;
                _opcodeNumSize = Types.SizeShort;
            } else {
                _opcodeNum = b0;
                _opcodeNumSize = Types.SizeByte;
            }
            pc += _opcodeNumSize;

            // read operands
            // This is really, really ugly, in order to make it fast, I had to
            // inline a lot of functions, which has almost no effect in Hotspot
            // but the difference is huge in Dalvik
            // Note that this section assumes that all instructions are in story
            // memory, if the story is doing weird stuff with self-modifying code,
            // shame on its author
            int addrModeOffset = pc;
            int numOperands = NumOperands[_opcodeNum];
            int nbytesNumOperands = numOperands / 2 + numOperands % 2;
            pc += nbytesNumOperands; // adjust pc to the start of operand data
            int numRead = 0;
            int byteVal = 0;
            Operand currentOperand = null;

            for (int i = 0; i < nbytesNumOperands; i++) {
                // **********************************************
                byteVal = storyBytes[addrModeOffset + i] & 0xff;
                // **********************************************
                currentOperand = _operands[numRead];
                currentOperand.addressMode = byteVal & 0x0f;

                // READ OPERAND START
                //_operands[numRead].value = readOperand(_operands[numRead].addressMode);
                switch (currentOperand.addressMode) {
                case 0:  currentOperand.value = 0;                 break; // ConstZero
                case 1: // ConstByte
                    // ******************************************
                    currentOperand.value = storyBytes[pc++] & 0xff;
                    // ******************************************
                    break;
                case 2:  currentOperand.value = nextShort(); break; // ConstShort
                case 3:  currentOperand.value = nextInt();   break; // ConstInt
                case 5: // Address00_FF
                    // ******************************************
                    currentOperand.value = storyBytes[pc++] & 0xff;
                    // ******************************************
                    break;
                case 6:  currentOperand.value = nextShort(); break; // Address0000_FFFF
                case 7:  currentOperand.value = nextInt();   break; // AddressAny
                case 8:  currentOperand.value = 0;           break; // Stack
                case 9: // Local00_FF
                    // ******************************************
                    currentOperand.value = storyBytes[pc++] & 0xff;
                    // ******************************************
                    break;
                case 10: currentOperand.value = nextShort(); break; // Local0000_FFFF
                case 11: currentOperand.value = nextInt();   break; // LocalAny
                case 13:
                    // ******************************************
                    currentOperand.value = storyBytes[pc++] & 0xff;                    
                    // ******************************************
                    break; // Ram00_FF
                case 14: currentOperand.value = nextShort(); break; // Ram0000_FFFF
                case 15: currentOperand.value = nextInt();   break; // RamAny
                default:
                    throw new IllegalArgumentException("unsupported address mode: " +
                                                       currentOperand.addressMode);
                }
                // READ OPERAND END

                numRead++;
                // second operand nibble in byte val
                if (numRead < numOperands) {
                    currentOperand = _operands[numRead];
                    currentOperand.addressMode = (byteVal >>> 4) & 0x0f;

                    // READ OPERAND START
                    // _operands[numRead].value = readOperand(_operands[numRead].addressMode);
                    switch (currentOperand.addressMode) {
                    case 0:  currentOperand.value = 0;                 break; // ConstZero
                    case 1: // ConstByte
                        // ******************************************
                        currentOperand.value = storyBytes[pc++] & 0xff;
                        // ******************************************
                        break;
                    case 2:  currentOperand.value = nextShort(); break; // ConstShort
                    case 3:  currentOperand.value = nextInt();   break; // ConstInt
                    case 5: // Address00_FF
                        // ******************************************
                        currentOperand.value = storyBytes[pc++] & 0xff;
                        // ******************************************
                        break;
                    case 6:  currentOperand.value = nextShort(); break; // Address0000_FFFF
                    case 7:  currentOperand.value = nextInt();   break; // AddressAny
                    case 8:  currentOperand.value = 0;                 break; // Stack
                    case 9: // Local00_FF
                        // ******************************************
                        currentOperand.value = storyBytes[pc++] & 0xff;
                        // ******************************************
                        break;
                    case 10: currentOperand.value = nextShort(); break; // Local0000_FFFF
                    case 11: currentOperand.value = nextInt();   break; // LocalAny
                    case 13: // Ram00_FF
                        // ******************************************
                        currentOperand.value = storyBytes[pc++] & 0xff;
                        // ******************************************
                        break;
                    case 14: currentOperand.value = nextShort(); break; // Ram0000_FFFF
                    case 15: currentOperand.value = nextInt();   break; // RamAny
                    default:
                        throw new IllegalArgumentException("unsupported address mode: " +
                                                           currentOperand.addressMode);
                    }
                    // READ OPERAND END
                    numRead++;
                }
            }
  
            // for debugging
            /*
              StringBuilder builder = new StringBuilder();
              builder.append("%04d: $%04x - @%s".format(iterations, pc, Opcodes.name(_opcodeNum)))
              int numOperands = Opcodes.numOperands(_opcodeNum);
              for (int i = 0; i < numOperands; i++) {
              builder.append(" %s".format(_operands(i).toString(state)));
              }
              builder.append(" {FP = %d SP = %d} ".format(_state.fp, _state.sp));
              builder.append(" " + state.stackValuesAsString);
              logger.info(builder.toString());
            */
            // Debugging end

            // execute instruction
            switch (_opcodeNum) {
            case 0x00: break; // nop, do nothing
            case 0x10: // add
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] + _operandValues[1]); break;
            case 0x11: // sub
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] - _operandValues[1]); break;
            case 0x12: // mul
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] * _operandValues[1]); break;
            case 0x13: // div
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] / _operandValues[1]); break;
            case 0x14: // mod
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] % _operandValues[1]); break;
            case 0x15: // neg
                storeAtOperand(1, -getOperand(0)); break;
            case 0x18: // bitand
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] & _operandValues[1]); break;
            case 0x19: // bitor
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] | _operandValues[1]); break;
            case 0x1a: // bitxor
                getOperandValues(2);
                storeAtOperand(2, _operandValues[0] ^ _operandValues[1]); break;
            case 0x1b: // bitnot
                storeAtOperand(1, ~getOperand(0)); break;
            case 0x1c: // shiftl
                {
                    getOperandValues(2);
                    int value    = _operandValues[0];
                    int numShift = _operandValues[1];
                    int result   = (numShift >= 32 || numShift < 0) ? 0 : (value << numShift);
                    storeAtOperand(2, result);
                }
                break;
            case 0x1d: // sshiftr
                {
                    getOperandValues(2);
                    int value    = _operandValues[0];
                    int numShift = _operandValues[1];
                    int result = 0;
                    if (value < 0 && (numShift >= 32 || numShift < 0))       result = -1;
                    else if (value >= 0 && (numShift >= 32 || numShift < 0)) result = 0;
                    else                                                     result = (value >> numShift);
                    storeAtOperand(2, result);
                }
                break;
            case 0x1e: // ushiftr
                {
                    getOperandValues(2);
                    int value    = _operandValues[0];
                    int numShift = _operandValues[1];
                    int result   = (numShift >= 32 || numShift < 0) ? 0 : (value >>> numShift);
                    storeAtOperand(2, result);
                }
                break;
            case 0x20: // jump
                doBranch(getOperand(0)); break;
            case 0x22: // jz
                getOperandValues(2);
                if (_operandValues[0] == 0) doBranch(_operandValues[1]); break;
            case 0x23: // jnz
                getOperandValues(2);
                if (_operandValues[0] != 0) doBranch(_operandValues[1]); break;
            case 0x24: // jeq
                getOperandValues(3);
                if (_operandValues[0] == _operandValues[1]) doBranch(_operandValues[2]); break;
            case 0x25: // jne
                getOperandValues(3);
                if (_operandValues[0] != _operandValues[1]) doBranch(_operandValues[2]); break;
            case 0x26: // jlt
                getOperandValues(3);
                if (_operandValues[0] < _operandValues[1]) doBranch(_operandValues[2]); break;
            case 0x27: // jge
                getOperandValues(3);
                if (_operandValues[0] >= _operandValues[1]) doBranch(_operandValues[2]); break;
            case 0x28: // jgt
                getOperandValues(3);
                if (_operandValues[0] > _operandValues[1]) doBranch(_operandValues[2]); break;
            case 0x29: // jle
                getOperandValues(3);
                if (_operandValues[0] <= _operandValues[1]) doBranch(_operandValues[2]); break;
            case 0x2a: // jltu
                {
                    getOperandValues(3);
                    long op0 = _operandValues[0] & 0x0ffffffffl;
                    long op1 = _operandValues[1] & 0x0ffffffffl;
                    if (op0 < op1) doBranch(_operandValues[2]);
                }
                break;
            case 0x2b: // jgeu
                {
                    getOperandValues(3);
                    long op0 = _operandValues[0] & 0x0ffffffffl;
                    long op1 = _operandValues[1] & 0x0ffffffffl;
                    if (op0 >= op1) doBranch(_operandValues[2]);
                }
                break;
            case 0x2c: // jgtu
                {
                    getOperandValues(3);
                    long op0 = _operandValues[0] & 0x0ffffffffl;
                    long op1 = _operandValues[1] & 0x0ffffffffl;
                    if (op0 > op1) doBranch(_operandValues[2]);
                }
                break;
            case 0x2d: // jleu
                {
                    getOperandValues(3);
                    long op0 = _operandValues[0] & 0x0ffffffffl;
                    long op1 = _operandValues[1] & 0x0ffffffffl;
                    if (op0 <= op1) doBranch(_operandValues[2]);
                }
                break;
            case 0x30: // call
                // Take the arguments from the stack and store them, prepareCall
                // will use them and store them according to the function type
                getOperandValues(2);
                int funaddr   = _operandValues[0];
                _numArguments = _operandValues[1];
                for (int i = 0; i < _numArguments; i++) {
                    _arguments[i] = popInt();
                }
                prepareCall(funaddr, _operands[2]);
                break;
            case 0x31: // return
                popCallStub(getOperand(0)); break;
            case 0x32: // catch
                // Pull the value from the stack if L1 is SP
                if (_operands[0].addressMode == AddressModes.Stack ||
                    _operands[1].addressMode == AddressModes.Stack) {
                    int branchOffset = getOperand(1);
                    pushCallStub(_operands[0]);
                    storeAtOperand(0, sp); // catch token
                    doBranch(branchOffset);
                } else {
                    pushCallStub(_operands[0]);
                    storeAtOperand(0, sp); // catch token
                    doBranch(getOperand(1));
                }
                break;
            case 0x33: // throw
                int storeVal   = getOperand(0);
                int catchToken = getOperand(1);
                logger.info(String.format("@throw %d %d CURRENT SP = %d",
                                          storeVal, catchToken, sp));
                if (sp < catchToken)
                    throw new IllegalStateException("@throw: catch token > current SP !!!");
                sp = catchToken;
                logger.info(String.format("@throw, SP is now: %d\n", sp));
                popCallStubThrow(storeVal);
                logger.info(String.format("@throw, after popCallStub SP is now: %d\n",
                                          sp));
                break;
            case 0x34: // tailcall
                tailCall(getOperand(0), getOperand(1)); break;
            case 0x40: // copy
                storeAtOperand(1, getOperand(0)); break;
            case 0x41: // copys
                storeAtOperand16(1, getOperand16(0) & 0xffff); break;
            case 0x42: // copyb
                storeAtOperand8(1, getOperand8(0) & 0xff); break;
            case 0x44: // sexs 
                storeAtOperand(1, Types.signExtend16(getOperand(0))); break;
            case 0x45: // sexb
                {
                    //storeAtOperand(1, Types.signExtend8(getOperand(0)));
                    int value = getOperand(0);
                    value = ((value & 0x80) == 0x80) ? value | 0xffffff00 : value & 0xff;
                    storeAtOperand(1, value);
                }
                break;
            case 0x48: // aload
                {
                    getOperandValues(2);
                    int arr   = _operandValues[0];
                    int index = _operandValues[1];
                    storeAtOperand(2, memIntAt(arr + index * 4));
                }
                break;
            case 0x49: // aloads
                {
                    getOperandValues(2);
                    int arr   = _operandValues[0];
                    int index = _operandValues[1];
                    storeAtOperand(2, memShortAt(arr + index * 2));
                }
                break;
            case 0x4a: // aloadb
                {
                    getOperandValues(2);
                    int arr    = _operandValues[0];
                    int index  = _operandValues[1];
                    storeAtOperand(2, memByteAt(arr + index));
                }
                break;
            case 0x4b: // aloadbit
                {
                    getOperandValues(2);
                    int addr      = _operandValues[0];
                    int bitOffset = _operandValues[1];
                    int memAddr   = addr + bitOffset / 8;
                    int bitnum    = bitOffset % 8;
                    if (bitnum < 0) {
                        // adjust bitnum if necessary
                        memAddr--;
                        bitnum += 8;
                    }
                    int mask = 1 << bitnum;
                    int test =
                        ((memByteAt(memAddr) & mask) == mask) ? 1 : 0;
                    storeAtOperand(2, test);
                }
                break;
            case 0x4c: // astore
                {
                    getOperandValues(3);
                    int arr   = _operandValues[0];
                    int index = _operandValues[1];
                    setMemIntAt(arr + index * 4, _operandValues[2]);
                }
                break;
            case 0x4d: // astores
                {
                    getOperandValues(3);
                    int arr   = _operandValues[0];
                    int index = _operandValues[1];
                    setMemShortAt(arr + index * 2, _operandValues[2]);
                }
                break;
            case 0x4e: // astoreb
                {
                    getOperandValues(3);
                    int arr   = _operandValues[0];
                    int index = _operandValues[1];
                    setMemByteAt(arr + index, _operandValues[2]);
                }
                break;
            case 0x4f: // astorebit
                {
                    getOperandValues(3);
                    int addr      = _operandValues[0];
                    int bitOffset = _operandValues[1];
                    int memAddr   = addr + bitOffset / 8;
                    int bitnum    = bitOffset % 8;
                    if (bitnum < 0) {
                        // adjust bitnum if necessary
                        memAddr--;
                        bitnum += 8;
                    }
                    if (_operandValues[2] == 0) { // clear
                        setMemByteAt(memAddr,
                                     memByteAt(memAddr) & (~(1 << bitnum) & 0xff));
                    } else { // set
                        setMemByteAt(memAddr,
                                     memByteAt(memAddr) | ((1 << bitnum) & 0xff));
                    }
                }
                break;
            case 0x50: // stkcount
                storeAtOperand(0, numStackValuesInCallFrame()); break;
            case 0x51: // stkpeek
                storeAtOperand(1, stackPeek(getOperand(0))); break;
            case 0x52: // stkswap
                stackSwap(); break;
            case 0x53: // stkroll
                stackRoll(getOperand(0), getOperand(1)); break;
            case 0x54: // stkcopy
                {
                    int numElems  = getOperand(0);
                    int copyStart = sp - Types.SizeInt * numElems;
                    for (int i = 0; i < numElems; i++) {
                        pushInt(getIntInStack(copyStart + i * Types.SizeInt));
                    }
                }
                break;
            case 0x70: // streamchar
                currentIOSystem.streamChar((char) (getOperand(0) & 0xff)); break;
            case 0x71: // streamnum
                currentIOSystem.streamNum(getOperand(0), 0); break;
            case 0x72: // streamstr
                currentIOSystem.streamStr(StreamStrState.newString(getOperand(0))); break;
            case 0x73: // streamunichar
                currentIOSystem.streamUniChar(getOperand(0)); break;
            case 0x100: // gestalt
                {
                    int selector = getOperand(0);
                    int arg      = getOperand(1);
                    if (selector == GlulxGestalt.MAllocHeap) {
                        storeAtOperand(2, heapStart());
                    } else {
                        storeAtOperand(2, GlulxGestalt.gestalt(selector, arg));
                    }
                }
                break;
            case 0x101: // debugtrap
                fatal(String.format("[** ERROR, VM HALTED WITH CODE %d **]", getOperand(0))); break;
            case 0x102: // getmemsize
                storeAtOperand(0, memsize()); break;
            case 0x103: // setmemsize
                int newSize = getOperand(0);
                logger.info(String.format("@setmemsize %d\n", newSize));
                if (newSize < endmem) fatal("@setmemsize: size must be >= ENDMEM");
                if (newSize % 256 != 0) fatal("@setmemsize: size must be multiple of 256");
                if (memheap.active()) {
                    fatal("@setmemsize: can not set while heap is active");
                }
                setMemsize(newSize);
                // Result is 0 for success, 1 for fail
                break;
            case 0x104: // jumpabs
                pc = getOperand(0); break;
            case 0x110: // random
                {
                    int range = getOperand(0);
                    if (range < 0) {
                        int translate = range + 1;
                        storeAtOperand(1, _random.nextInt(-range) + translate);
                    } else if (range == 0) {
                        storeAtOperand(1, _random.nextInt());
                    } else {
                        storeAtOperand(1, _random.nextInt(range));
                    }
                }
                break;
            case 0x111: // setrandom
                {
                    int seed = getOperand(0);
                    if (seed == 0) _random.setSeed(seed);
                    else _random.setSeed(System.currentTimeMillis());
                }
                break;
            case 0x120: // quit
                pRunState = VMRunStates.Halted; break;
            case 0x121: // verify
                storeAtOperand(0, verify()); break;
            case 0x122: // restart
                restart(); break;
            case 0x123: // save
                {
                    int streamId = getOperand(0);
                    SaveGameWriter writer = new SaveGameWriter(glk, streamId, this, _operands[1]);
                    boolean result = writer.writeGameFile();
                    if (result) storeAtOperand(1, 0);
                    else storeAtOperand(1, 1);
                }
                break;
            case 0x124: // restore
                {
                    int streamId = getOperand(0);
                    SaveGameLoader loader = new SaveGameLoader(glk, streamId, this, _originalRam);
                    if (loader.loadGame()) {
                        popCallStubThrow(-1);
                    } else {
                        storeAtOperand(1, 1); // fail for now
                    }
                }
                break;
            case 0x125: // saveundo
                _undoSnapshots.add(createSnapshot(_operands[0]));
                storeAtOperand(0, 0); // Always say SUCCEED
                break;
            case 0x126: // restoreundo
                if (_undoSnapshots.size() > 0) {
                    readSnapshot(_undoSnapshots.get(_undoSnapshots.size() - 1),
                                 _protectionStart,
                                       _protectionLength);
                    _undoSnapshots.remove(_undoSnapshots.size() - 1);
                    popCallStubThrow(-1);
                } else {
                    storeAtOperand(0, 1); // fail
                }
                logger.info(String.format("RESTORED WITH PC: %02x AND FP: %d SP: %d",
                                          pc, fp, sp));
                break;
            case 0x127: // protect
                _protectionStart  = getOperand(0);
                _protectionLength = getOperand(1);
                break;
            case 0x130: // glk
                {
                    getOperandValues(2);
                    int glkId   = _operandValues[0];
                    int numArgs = _operandValues[1];
                    int[] args  = new int[numArgs];
                    for (int i = 0; i < numArgs; i++) {
                        args[i] = popInt();
                    }
                    int glkResult = _glkDispatch.dispatch(glkId, args);
                    //printf("GLK result = #$%02x\n", glkResult)
                    storeAtOperand(2, glkResult);
                }
                break;
            case 0x140: // getstringtbl
                storeAtOperand(0, currentDecodingTable); break;
            case 0x141: // setstringtbl
                {
                    int newDecodingTable = getOperand(0);
                    currentDecodingTable = newDecodingTable;
                    if (newDecodingTable == 0) {
                        logger.warning("CUSTOM DECODING TABLE SET TO 0 !!!");
                    }
                }
                break;
            case 0x148: // getiosys
                storeAtOperand(0, currentIOSystem.id());
                storeAtOperand(1, currentIOSystem.rock);
                break;
            case 0x149: // setiosys
                {
                    getOperandValues(2);
                    int iosys = _operandValues[0];
                    int rock  = _operandValues[1];
                    //currentIOSystem = (iosys: @switch) match {
                    switch (iosys) {
                    case 0:  currentIOSystem = new NullIOSystem(this, rock); break;
                    case 1:  currentIOSystem = new FilterIOSystem(this, rock); break;
                    case 2:  currentIOSystem = new GlkIOSystem(this, glk, rock); break;
                    case 20: currentIOSystem = new ChannelIOSystem(this, rock); break;
                    default:
                        throw new UnsupportedOperationException(String.format("IO system[%d] not supported",
                                                                              iosys));
                    }
                }
                break;
            case 0x150: // linearsearch
                {
                    getOperandValues(7);
                    int result = linearSearch.apply(_operandValues[0], _operandValues[1],
                                                    _operandValues[2], _operandValues[3],
                                                    _operandValues[4], _operandValues[5],
                                                    _operandValues[6]);
                    storeAtOperand(7, result);
                }
                break;
            case 0x151: // binarysearch
                {
                    getOperandValues(7);
                    int result = binarySearch.apply(_operandValues[0], _operandValues[1],
                                                    _operandValues[2], _operandValues[3],
                                                    _operandValues[4], _operandValues[5],
                                                    _operandValues[6]);
                    storeAtOperand(7, result);
                }
                break;
            case 0x152: // linkedsearch
                {
                    getOperandValues(6);
                    int result = linkedSearch.apply(_operandValues[0], _operandValues[1],
                                                    _operandValues[2], _operandValues[3],
                                                    _operandValues[4], _operandValues[5]);
                    storeAtOperand(6, result);
                }
                break;
            case 0x160: // callf
                doCallf0(getOperand(0), _operands[1]); break;
            case 0x161: // callfi
                doCallf1(getOperand(0), _operands[2], getOperand(1)); break;
            case 0x162: // callfii
                doCallf2(getOperand(0), _operands[3], getOperand(1), getOperand(2)); break;
            case 0x163: // callfiii
                doCallf3(getOperand(0), _operands[4], getOperand(1), getOperand(2),
                         getOperand(3)); break;
            case 0x170: // mzero
                mzero(getOperand(0), getOperand(1)); break;
            case 0x171: // mcopy
                mcopy(getOperand(0), getOperand(1), getOperand(2)); break;
            case 0x178: // malloc
                storeAtOperand(1, malloc(getOperand(0))); break;
            case 0x179: // mfree
                mfree(getOperand(0)); break;
            case 0x180: // accelfunc
                _accelSystem.setFunction(getOperand(0), getOperand(1)); break;
            case 0x181: // accelparam
                _accelSystem.setParameter(getOperand(0), getOperand(1)); break;
            case 0x190: // numtof
                {
                    float operand1 = (float) getOperand(0);
                    storeAtOperand(1, Float.floatToRawIntBits(operand1));
                }
                break;
            case 0x191: // ftonumz
                storeAtOperand(1, GlulxFloat.ftonumz(getOperand(0))); break;
            case 0x192: // ftonumn
                storeAtOperand(1, GlulxFloat.ftonumn(getOperand(0))); break;
            case 0x198: // ceil
                storeAtOperand(1, GlulxFloat.ceil(getOperand(0))); break;
            case 0x199: // floor
                storeAtOperand(1, GlulxFloat.floor(getOperand(0))); break;
            case 0x1a0: // fadd
                storeAtOperand(2, GlulxFloat.fadd(getOperand(0), getOperand(1))); break;
            case 0x1a1: // fsub
                storeAtOperand(2, GlulxFloat.fsub(getOperand(0), getOperand(1))); break;
            case 0x1a2: // fmul
                storeAtOperand(2, GlulxFloat.fmul(getOperand(0), getOperand(1))); break;
            case 0x1a3: // fdiv
                storeAtOperand(2, GlulxFloat.fdiv(getOperand(0), getOperand(1))); break;
            case 0x1a4: // fmod
                {
                    int operand1 = getOperand(0);
                    int operand2 = getOperand(1);
                    storeAtOperand(2, GlulxFloat.fmodRemainder(operand1, operand2));
                    storeAtOperand(3, GlulxFloat.fmodQuotient(operand1, operand2));
                }
                break;
            case 0x1a8: // sqrt
                storeAtOperand(1, GlulxFloat.sqrt(getOperand(0))); break;
            case 0x1a9: // exp
                storeAtOperand(1, GlulxFloat.exp(getOperand(0))); break;
            case 0x1aa: // log
                storeAtOperand(1, GlulxFloat.log(getOperand(0))); break;
            case 0x1ab: // pow
                storeAtOperand(2, GlulxFloat.pow(getOperand(0), getOperand(1))); break;
            case 0x1b0: // sin
                storeAtOperand(1, GlulxFloat.sin(getOperand(0))); break;
            case 0x1b1: // cos
                storeAtOperand(1, GlulxFloat.cos(getOperand(0))); break;
            case 0x1b2: // tan
                storeAtOperand(1, GlulxFloat.tan(getOperand(0))); break;
            case 0x1b3: // asin
                storeAtOperand(1, GlulxFloat.asin(getOperand(0))); break;
            case 0x1b4: // acos
                storeAtOperand(1, GlulxFloat.acos(getOperand(0))); break;
            case 0x1b5: // atan
                storeAtOperand(1, GlulxFloat.atan(getOperand(0))); break;
            case 0x1b6: // atan2
                storeAtOperand(2, GlulxFloat.atan2(getOperand(0), getOperand(1))); break;
            case 0x1c0: // jfeq
                if (GlulxFloat.feq(getOperand(0), getOperand(1), getOperand(2))) {
                    doBranch(getOperand(3));
                }
                break;
            case 0x1c1: // jfne
                if (GlulxFloat.fne(getOperand(0), getOperand(1), getOperand(2))) {
                    doBranch(getOperand(3));
                }
                break;
            case 0x1c2: // jflt
                if (GlulxFloat.flt(getOperand(0), getOperand(1))) {
                    doBranch(getOperand(2));
                }
                break;
            case 0x1c3: // jfle
                if (GlulxFloat.fle(getOperand(0), getOperand(1))) doBranch(getOperand(2)); break;
            case 0x1c4: // jfgt
                if (GlulxFloat.fgt(getOperand(0), getOperand(1))) doBranch(getOperand(2)); break;
            case 0x1c5: // jfge
                if (GlulxFloat.fge(getOperand(0), getOperand(1))) doBranch(getOperand(2)); break;
            case 0x1c8: // jisnan
                if (GlulxFloat.isNaN(getOperand(0))) doBranch(getOperand(1)); break;
            case 0x1c9: // jisinf
                if (GlulxFloat.isInfinity(getOperand(0))) doBranch(getOperand(1)); break;
            default:
                throw new IllegalArgumentException(String.format("unknown opcode number: %02x",
                                                                 _opcodeNum));
            }
            //iterations++;
        }
        long elapsed = System.currentTimeMillis() - startTime;
        System.out.printf("Executed turn in %d ms.\n", (int) elapsed);
    }


    public void fatal(String msg) {
        glk.put_java_string(msg);
        pRunState = VMRunStates.Halted;
    }
    
    // *******************************************************************************
    // *******************************************************************************
    // *******************************************************************************
    // *******************************************************************************
    // STATE MANAGEMENT FUNCTIONS (FORMERLY KNOWN AS GLULXVMSTATE)
    // *******************************************************************************
    // *******************************************************************************
    // *******************************************************************************
    // *******************************************************************************

    public boolean isGlulx() { return storyIntAt(0) == 0x476c756c; } // 'Glul'
    public int version() { return storyIntAt(4); }
    private int storyIntAt(int addr) {
        return ((storyBytes[addr] & 0xff) << 24) | ((storyBytes[addr + 1] & 0xff) << 16) |
            ((storyBytes[addr + 2] & 0xff) << 8) | (storyBytes[addr + 3] & 0xff);
    }

    public int runState() { return pRunState; }
    public void setRunState(int state) { this.pRunState = state; }
  
    private void protectedMemRestore(byte[] memarray, int srcOffset,
                                     int destOffset, int numBytes,
                                     int protectionStart,
                                     int protectionLength) {
        if (protectionLength > 0) {
            for (int i = 0; i < numBytes; i++) {                
                int destAddress = destOffset + i;
                if (destAddress < protectionStart ||
                    destAddress >= protectionStart + protectionLength) {
                    storyBytes[destAddress] = (byte) memarray[i];
                }
            }
        } else {
            //_story.copyBytesFrom(memarray, srcOffset, destOffset, numBytes)
            System.arraycopy(memarray, srcOffset, storyBytes, destOffset, numBytes);
        }
    }

    private void _setExtendedMem(int size) {
        int currentExtSize = (extMem == null) ? 0 : _extEnd - extstart;
        // note: we actually do not need to "shrink" extended memory, we only
        // need to extend it
        if (currentExtSize != size) {
            if (size > currentExtSize) {
                byte[] extbytes = new byte[size];
                if (extMem != null) {
                    extMem.copyBytesTo(extbytes, extstart, currentExtSize);
                }
                extMem = new DefaultMemory(extbytes, extstart);
            }
        }
        _extEnd = extstart + size;
    }

    public int ramSize() { return memsize() - ramstart; }

    private boolean inStoryMem(int addr) { return addr < extstart; }
    public boolean inExtMem(int addr)   { return addr >= extstart && addr < _extEnd; }
    private boolean fitsInStoryMem(int addr, int size) {
        return inStoryMem(addr) && inStoryMem(addr + size - 1);
    }

    private boolean fitsOnHeap(int addr, int size) {
        return memheap.memblockAt(addr) != null;
    }

    // Stack interface
    public boolean stackEmpty() { return sp == 0; }

    public int localsPos() { return getIntInStack(fp + OffsetLocalsPos); }
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
        if (addr < extstart) return (storyBytes[addr] & 0xff); // inStoryMem(), manually inlined
        else if (inExtMem(addr)) return extMem.byteAt(addr);
        else return memheap.byteAt(addr);
    }

    public void setMemByteAt(int addr, int value) {
        if (addr < ramstart) {
            logger.warning(String.format("SETTING BYTE VALUE IN ROM %02x = %d !",
                                         addr, value));
        }
        if (addr < extstart)     storyBytes[addr] = (byte) (value & 0xff);
        else if (inExtMem(addr)) extMem.setByteAt(addr, value);
        else                     memheap.setByteAt(addr, value);
    }

    public int memShortAt(int addr) {
        if (addr < extstart) {
            return ((storyBytes[addr] & 0xff) << 8) | (storyBytes[addr + 1] & 0xff);
        } else if (inExtMem(addr)) return extMem.shortAt(addr);
        else return memheap.shortAt(addr);
    }

    public void setMemShortAt(int addr, int value) {
        if (addr < ramstart) {
            logger.warning(String.format("SETTING SHORT VALUE IN ROM %02x = %d !",
                                         addr, value));
        }
        if (addr < extstart) {
            storyBytes[addr]     = (byte) ((value >>> 8) & 0xff);
            storyBytes[addr + 1] = (byte) (value & 0xff);
        } else if (inExtMem(addr))   extMem.setShortAt(addr, value);
        else                       memheap.setShortAt(addr, value);
    }

    public int memIntAt(int addr) {
        if (addr < extstart) {
            return ((storyBytes[addr] & 0xff) << 24) | ((storyBytes[addr + 1] & 0xff) << 16) |
                ((storyBytes[addr + 2] & 0xff) << 8) | (storyBytes[addr + 3] & 0xff);
        } else if (inExtMem(addr)) return extMem.intAt(addr);
        else return memheap.intAt(addr);
    }

    public void setMemIntAt(int addr, int value) {
        if (addr < ramstart) {
            logger.warning(String.format("SETTING INT VALUE IN ROM %02x = %d !",
                                         addr, value));
        }
        if (addr < extstart) {
            storyBytes[addr]     = (byte) ((value >>> 24) & 0xff);
            storyBytes[addr + 1] = (byte) ((value >>> 16) & 0xff);
            storyBytes[addr + 2] = (byte) ((value >>> 8) & 0xff);
            storyBytes[addr + 3] = (byte) (value & 0xff);
        } else if (inExtMem(addr)) extMem.setIntAt(addr, value);
        else                       memheap.setIntAt(addr, value);
    }

    public int ramByteAt(int address) { return memByteAt(ramstart + address); }
    public void setRamByteAt(int address, int value) {
        setMemByteAt(ramstart + address, value);
    }
    public int ramShortAt(int address) { return memShortAt(ramstart + address); }
    public void setRamShortAt(int address, int value) {
        setMemShortAt(ramstart + address, value);
    }
    public int ramIntAt(int address) { return memIntAt(ramstart + address); }
    public void setRamIntAt(int address, int value) {
        setMemIntAt(ramstart + address, value);
    }

    public int malloc(int size) { return memheap.allocate(size); }
    public void mfree(int addr) { memheap.free(addr); }
    public void mcopy(int numBytes, int srcAddr, int destAddr) {
        if (fitsInStoryMem(srcAddr, numBytes) &&
            fitsInStoryMem(destAddr, numBytes)) {
            //_story.copyBytesTo(destAddr, srcAddr, numBytes);
            System.arraycopy(storyBytes, srcAddr, storyBytes, destAddr, numBytes);
        } else if (fitsOnHeap(srcAddr, numBytes) && fitsOnHeap(destAddr, numBytes)) {
            memheap.copyBytesTo(destAddr, srcAddr, numBytes);
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
    public int memsize() { return (memheap.active()) ? memheap.maxAddress() : _extEnd; }
    public void setMemsize(int newSize) { _setExtendedMem(newSize - extstart); }
    public int heapStart() { return (memheap.active()) ? _extEnd : 0; }

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
        int lposAddr = fp + OffsetLocalsPos;
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
        int descriptorPos    = fp + OffsetLocalsFormat;
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
        int descriptorPos = fp + OffsetLocalsFormat;
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
        int ramsize = extstart - ramstart;
        protectedMemRestore(snapshot.ram, 0, ramstart, ramsize,
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
        byte[] extmemClone = null;

        // TODO: extmem and heap
        return new Snapshot(ram, stackValues, extmemClone);
    }

    public byte[] cloneRam() {
        int ramsize = extstart - ramstart;
        logger.info(String.format("Copying %d Bytes of RAM to preserve initial data", ramsize));
        byte[] ram = new byte[ramsize];
        //_story.copyBytesTo(ram, ramstart, ramsize)
        System.arraycopy(storyBytes, ramstart, ram, 0, ramsize);
        return ram;
    }
}
