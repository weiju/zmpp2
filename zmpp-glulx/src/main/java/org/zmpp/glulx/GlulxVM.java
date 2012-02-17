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
import java.util.*;
import java.util.logging.*;

/****************************************************************************
 ****
 **** VM main control
 ****
 */
public class GlulxVM {
    private static final int MaxLocalDescriptors = 255;
    private static final int MaxOperands         = 10;
    private static final int MaxArguments        = 20;
    private static final int SizeLocalDescriptor = Types.SizeByte * 2;

    private static Logger logger = Logger.getLogger("glulx");
    private int iterations = 1;

    // State is public for subsystems to access. This is the only data
    // that is needed to be serializaed between turns. All other data
    // is scratch data which is only valid during one turn.
    public GlulxVMState state = new GlulxVMState();

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
    private int _opcodeNum;
    private int _opcodeNumSize;

    // function arguments - we avoid creating them over and over
    // we are using these when we setup a normal function call and
    // we also use them in accelerated functions
    private int[] _arguments    = new int[MaxArguments];
    private int _numArguments;
  
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
    public int runState() { return state.pRunState; }
    public BlorbData blorbData;
    public int currentDecodingTable;
    private IOSystem currentIOSystem = new NullIOSystem(this, 0);

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
        glk = new Glk(new EventManager(state));
        _glkDispatch = new GlkDispatch(state, glk);
        state.init(storyBytes);
        currentDecodingTable  = state.header.decodingTable();
        _accelSystem.glk = glk;
        if (_originalRam == null) _originalRam = state.cloneRam();

        prepareCall(state.header.startfunc(), null);
    }

    private void restart() {
        state.restart(_originalRam, _protectionStart, _protectionLength);
        //_protectionStart  = 0
        //_protectionLength = 0
        //_undoSnapshots = Nil
        prepareCall(state.header.startfunc(), null);
    }

    private void printState() { System.out.println(state.toString()); }

    private int readLocalDescriptors(int addr) {
        int currentAddr = addr;
        int descIndex = 0;
        boolean hasMoreDescriptors = true;

        while (hasMoreDescriptors) {
            _localDescriptors[descIndex].localType  = state.memByteAt(currentAddr);
            _localDescriptors[descIndex].localCount = state.memByteAt(currentAddr + 1);

            hasMoreDescriptors = !(_localDescriptors[descIndex].localType == 0 &&
                                   _localDescriptors[descIndex].localCount == 0);
            descIndex++;
            currentAddr += SizeLocalDescriptor;
        }
        // include the terminating pair in the count
        return descIndex;
    }

    private int setLocalDescriptorsToCallFrame(int numDescriptors) {
        for (int i = 0; i < numDescriptors; i++) {
            state.pushByte(_localDescriptors[i].localType);
            state.pushByte(_localDescriptors[i].localCount);
        }
        // Ensure a size dividable by 4 (the size of an int)
        int localDescriptorSize = numDescriptors * Types.SizeShort;
        if ((localDescriptorSize % Types.SizeInt) != 0) {
            state.pushShort(0);
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
            if (ltype == Types.ShortType && ((state.sp & 0x01) == 1)) {
                state.pushByte(0);
                numPadBytes = 1;
            } else if (ltype == Types.IntType && ((state.sp & 0x03) != 0)) {
                numPadBytes = Types.SizeInt - (state.sp & 0x03);
                for (int j = 0; j < numPadBytes; j++) state.pushByte(0);
            }
            // push numlocals locals of size ltype on the stack, we do this
            // by incrementing the stackpointer, which does not do any initialization
            // to the variables
            int blocksize = numlocals * ltype;
            for (int j = 0; j < blocksize; j++) state.pushByte(0);
            localSectionSize += blocksize + numPadBytes;
        }
        return localSectionSize;
    }

    // Used by both signed and unsigned instructions.
    private int getOperand(int pos) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: return 0; // ConstZero
        case 1: return Types.signExtend8(operand.value);  // ConstByte
        case 2: return Types.signExtend16(operand.value); // ConstShort
        case 3: return operand.value;                     // ConstInt
        case 7: return state.memIntAt(operand.value);     // AddressAny
        case 8: return state.popInt();                    // Stack
        case 9: case 10: case 11: // Local00_FF, Local0000_FFFF, LocalAny
            return state.getLocalAtAddress(operand.value);
        case 13: case 14: case 15: // Ram00_FF, Ram0000_FFFF, RamAny
            return state.ramIntAt(operand.value);
        default:
            throw new IllegalStateException("unsupported operand type: " +
                                            operand.addressMode);
        }
    }

    // only for copyb/copys
    // Only used by copyb.
    private int getOperand8(int pos) {
        Operand operand = _operands[pos];
        switch (operand.addressMode) {
        case 0: return 0; // ConstZero
        case 1: return Types.signExtend8(operand.value);  // ConstByte
        case 2: return Types.signExtend16(operand.value); // ConstShort
        case 3: return operand.value;                     // ConstInt
        case 5: case 6: case 7:    // Address00_FF/Address0000_FFFF/AddressAny
            return state.memByteAt(operand.value);
        case 8: return state.popInt();                    // Stack
        case 9: case 10: case 11:  // Local00_FF/Local0000_FFFF/LocalAny 
            return state.getLocalByteAtAddress(operand.value);
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            return state.ramByteAt(operand.value);
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
        case 1: return Types.signExtend8(operand.value); // ConstByte
        case 2: return Types.signExtend16(operand.value); // ConstShort
        case 3: return operand.value; // ConstInt
        case 5: case 6: case 7: // Address00_FF/Address_0000_FFFF/AddressAny
            return state.memShortAt(operand.value);
        case 8: return state.popInt(); // Stack
        case 9: case 10: case 11: // Local00_FF/Local0000_FFFF/LocalAny
            return state.getLocalShortAtAddress(operand.value);
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            return state.ramShortAt(operand.value);
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
            state.setMemIntAt(operand.value, value);
            break;
        case 8:
            state.pushInt(value); // Stack
            break;
        case 9: case 10: case 11: // Local00_FF, Local0000_FFFF, LocalAny
            state.setLocalAtAddress(operand.value, value);
            break;
        case 13: case 14: case 15: // Ram00_FF, Ram0000_FFFF, RamAny
            state.setRamIntAt(operand.value, value);
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
            state.setMemByteAt(operand.value, value); break;
        case 8: state.pushInt(value); break; // Stack
        case 9: case 10: case 11: // Local00_FF/Local0000_FFFF/LocalAny
            state.setLocalByteAtAddress(operand.value, value); break;
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            state.setRamByteAt(operand.value, value); break;
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
            state.setMemShortAt(operand.value, value); break;
        case 8: state.pushInt(value); break; // Stack
        case 9: case 10: case 11: // Local00_FF/Local0000_FFFF/LocalAny
            state.setLocalShortAtAddress(operand.value, value); break;
        case 13: case 14: case 15: // Ram00_FF/Ram0000_FFFF/RamAny
            state.setRamShortAt(operand.value, value); break;
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
        state.pushInt(0); // call frame size
        state.pushInt(0); // locals position

        int funtype = state.memByteAt(funaddr);
        int numDescriptors = readLocalDescriptors(funaddr + 1);
        int localDescriptorSize = setLocalDescriptorsToCallFrame(numDescriptors);

        // now that we know the size of the local descriptors section, we set
        // the position of locals
        state.setIntInStack(state.fp + Stack.OffsetLocalsPos,
                            localDescriptorSize + 8);
        int localSectionSize = setLocalsToCallFrame(numDescriptors);

        if (funtype == 0xc0) { // stack-arg type
            // push arguments backwards, then the number of arguments
            for (int i = 0; i < _numArguments; i++) {
                state.pushInt(_arguments[_numArguments - i - 1]);
            }
            state.pushInt(_numArguments);
        } else if (funtype == 0xc1) { // local-arg type
            // Copy arguments on the stack backwards to the locals
            for (int i = 0; i < _numArguments; i++) {
                state.setLocal(i, _arguments[i]);
            }
        } else {
            throw new IllegalArgumentException(String.format("unsupported function type: %02x",
                                                             funtype));
        }

        // set frame len
        state.setIntInStack(state.fp, state.localsPos() + localSectionSize);
        // jump to the code
        state.pc = funaddr + 1 + SizeLocalDescriptor * numDescriptors;
    }
    // normal function call, called by the VM itself
    private void prepareCall(int funaddr, Operand storeLocation) {
        if (storeLocation != null) {
            state.pushCallStub(storeLocation);
            state.fp = state.sp;
        }
        if (_accelSystem.isAccelerated(funaddr)) {
            // 4 dummy ints on the stack to trick the check
            state.pushInt(0);
            state.pushInt(0);
            state.pushInt(0);
            state.pushInt(0);
            _accelSystem.call(funaddr, _arguments, _numArguments);
        } else {
            callFunction(funaddr);
        }
    }
  
    /*
  // generic function call, called by string decoding
  def prepareCall(funaddr : Int, destType: Int, destAddr: Int) {
    _state.pushCallStub(destType, destAddr)
    _state.fp = _state.sp
    callFunction(funaddr)
  }
    */

    // called by @tailcall
    private void tailCall(int funaddr, int numArgs) {
        _numArguments = numArgs;
        for (int i = 0; i < _numArguments; i++) {
            _arguments[i] = state.popInt();
        }
        state.sp = state.fp;
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
       state.pushCallStub(destType, destAddr, pcVal, fpVal);
       state.fp = state.sp;
       callFunction(funaddr);
   }

    public void callWithArgs(int destType, int destAddr, int pcVal, int fpVal,
                             int funaddr, int arg0) {
        _arguments[0] = arg0;
        _numArguments = 1;
        state.pushCallStub(destType, destAddr, pcVal, fpVal);
        state.fp = state.sp;
        callFunction(funaddr);
    }

    public void callWithArgsNoCallStub(int funaddr, int[] args) {
        for (int i = 0; i < args.length; i++) {
            _arguments[i] = args[i];
        }
        _numArguments = args.length;
        state.fp = state.sp;
        callFunction(funaddr);
    }
    public void callWithArgsNoCallStub(int funaddr) {
        _numArguments = 0;
        state.fp = state.sp;
        callFunction(funaddr);
    }

    public void callWithArgsNoCallStub(int funaddr, int arg0) {
        _arguments[0] = arg0;
        _numArguments = 1;
        state.fp = state.sp;
        callFunction(funaddr);
    }

    // Returns from a function
    public void popCallStub(int retval) {
        if (state.sp < state.fp + 12) {
            throw new IllegalStateException("popCallStub(), stack is too small !!");
        }
        state.sp = state.fp;
        if (state.sp == 0) {
            // return from entry function -> Quit
            state.pRunState = VMRunStates.Halted;
        } else {
            // we can't use GlulxVM's popInt(), because it performs checks on
            // the call frame, which is exactly what we manipulate here
            int fpValue  = state.popIntUnchecked();
            int pcValue  = state.popIntUnchecked();
            int destAddr = state.popIntUnchecked();
            int destType = state.popIntUnchecked();
            if (DestTypes.isStringDestType(destType)) {
                handleStringCallStub(destType, destAddr, pcValue, fpValue);
            } else { // regular behaviour
                state.fp = fpValue;
                state.pc = pcValue;
                state.storeResult(destType, destAddr, retval);
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
        else state.pc += offset - 2;
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
        while (state.pRunState == VMRunStates.Running) {
            //int pc = _state.pc;

            // decode opcode number
            // look at the two highest bits: 11 means 4 bytes, 10 means 2 bytes
            // else one byte
            int b0 = state.memByteAt(state.pc) & 0xff;
            int bitpattern = b0 & 0xc0;

            if (bitpattern == 0xc0) {
                _opcodeNum = state.memIntAt(state.pc) - 0xc0000000;
                _opcodeNumSize = Types.SizeInt;
            } else if (bitpattern == 0x80) {
                _opcodeNum = (state.memShortAt(state.pc) & 0xffff) - 0x8000;
                _opcodeNumSize = Types.SizeShort;
            } else {
                _opcodeNum = b0;
                _opcodeNumSize = Types.SizeByte;
            }
            state.pc += _opcodeNumSize;

            // read operands
            int addrModeOffset = state.pc;
            int numOperands = Opcodes.numOperands(_opcodeNum);
            int nbytesNumOperands = numOperands / 2 + numOperands % 2;
            state.pc += nbytesNumOperands; // adjust pc to the start of operand data
            int numRead = 0;
            int byteVal = 0;
            Operand currentOperand = null;
            for (int i = 0; i < nbytesNumOperands; i++) {
                byteVal = state.memByteAt(addrModeOffset + i);
                currentOperand = _operands[numRead];
                currentOperand.addressMode = byteVal & 0x0f;

                // READ OPERAND START
                //_operands[numRead].value = readOperand(_operands[numRead].addressMode);
                switch (currentOperand.addressMode) {
                case 0:  currentOperand.value = 0;                 break; // ConstZero
                case 1:  currentOperand.value = state.nextByte();  break; // ConstByte
                case 2:  currentOperand.value = state.nextShort(); break; // ConstShort
                case 3:  currentOperand.value = state.nextInt();   break; // ConstInt
                case 5:  currentOperand.value = state.nextByte();  break; // Address00_FF
                case 6:  currentOperand.value = state.nextShort(); break; // Address0000_FFFF
                case 7:  currentOperand.value = state.nextInt();   break; // AddressAny
                case 8:  currentOperand.value = 0;                 break; // Stack
                case 9:  currentOperand.value = state.nextByte();  break; // Local00_FF
                case 10: currentOperand.value = state.nextShort(); break; // Local0000_FFFF
                case 11: currentOperand.value = state.nextInt();   break; // LocalAny
                case 13: currentOperand.value = state.nextByte();  break; // Ram00_FF
                case 14: currentOperand.value = state.nextShort(); break; // Ram0000_FFFF
                case 15: currentOperand.value = state.nextInt();   break; // RamAny
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
                    case 1:  currentOperand.value = state.nextByte();  break; // ConstByte
                    case 2:  currentOperand.value = state.nextShort(); break; // ConstShort
                    case 3:  currentOperand.value = state.nextInt();   break; // ConstInt
                    case 5:  currentOperand.value = state.nextByte();  break; // Address00_FF
                    case 6:  currentOperand.value = state.nextShort(); break; // Address0000_FFFF
                    case 7:  currentOperand.value = state.nextInt();   break; // AddressAny
                    case 8:  currentOperand.value = 0;                 break; // Stack
                    case 9:  currentOperand.value = state.nextByte();  break; // Local00_FF
                    case 10: currentOperand.value = state.nextShort(); break; // Local0000_FFFF
                    case 11: currentOperand.value = state.nextInt();   break; // LocalAny
                    case 13: currentOperand.value = state.nextByte();  break; // Ram00_FF
                    case 14: currentOperand.value = state.nextShort(); break; // Ram0000_FFFF
                    case 15: currentOperand.value = state.nextInt();   break; // RamAny
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
                storeAtOperand(2, getOperand(0) + getOperand(1)); break;
            case 0x11: // sub
                storeAtOperand(2, getOperand(0) - getOperand(1)); break;
            case 0x12: // mul
                storeAtOperand(2, getOperand(0) * getOperand(1)); break;
            case 0x13: // div
                storeAtOperand(2, getOperand(0) / getOperand(1)); break;
            case 0x14: // mod
                storeAtOperand(2, getOperand(0) % getOperand(1)); break;
            case 0x15: // neg
                storeAtOperand(1, -(getOperand(0))); break;
            case 0x18: // bitand
                storeAtOperand(2, getOperand(0) & getOperand(1)); break;
            case 0x19: // bitor
                storeAtOperand(2, getOperand(0) | getOperand(1)); break;
            case 0x1a: // bitxor
                storeAtOperand(2, getOperand(0) ^ getOperand(1)); break;
            case 0x1b: // bitnot
                storeAtOperand(1, ~getOperand(0)); break;
            case 0x1c: // shiftl
                {
                    int value    = getOperand(0);
                    int numShift = getOperand(1);
                    int result   = (numShift >= 32 || numShift < 0) ? 0 : (value << numShift);
                    storeAtOperand(2, result);
                }
                break;
            case 0x1d: // sshiftr
                {
                    int value    = getOperand(0);
                    int numShift = getOperand(1);
                    int result = 0;
                    if (value < 0 && (numShift >= 32 || numShift < 0))       result = -1;
                    else if (value >= 0 && (numShift >= 32 || numShift < 0)) result = 0;
                    else                                                     result = (value >> numShift);
                    storeAtOperand(2, result);
                }
                break;
            case 0x1e: // ushiftr
                {
                    int value    = getOperand(0);
                    int numShift = getOperand(1);
                    int result   = (numShift >= 32 || numShift < 0) ? 0 : (value >>> numShift);
                    storeAtOperand(2, result);
                }
                break;
            case 0x20: // jump
                doBranch(getOperand(0)); break;
            case 0x22: // jz
                if (getOperand(0) == 0) doBranch(getOperand(1)); break;
            case 0x23: // jnz
                if (getOperand(0) != 0) doBranch(getOperand(1)); break;
            case 0x24: // jeq
                if (getOperand(0) == getOperand(1)) doBranch(getOperand(2)); break;
            case 0x25: // jne
                if (getOperand(0) != getOperand(1)) doBranch(getOperand(2)); break;
            case 0x26: // jlt
                if (getOperand(0) < getOperand(1)) doBranch(getOperand(2)); break;
            case 0x27: // jge
                if (getOperand(0) >= getOperand(1)) doBranch(getOperand(2)); break;
            case 0x28: // jgt
                if (getOperand(0) > getOperand(1)) doBranch(getOperand(2)); break;
            case 0x29: // jle
                if (getOperand(0) <= getOperand(1)) doBranch(getOperand(2)); break;
            case 0x2a: // jltu
                {
                    long op0 = getOperand(0) & 0x0ffffffffl;
                    long op1 = getOperand(1) & 0x0ffffffffl;
                    if (op0 < op1) doBranch(getOperand(2));
                }
                break;
            case 0x2b: // jgeu
                {
                    long op0 = getOperand(0) & 0x0ffffffffl;
                    long op1 = getOperand(1) & 0x0ffffffffl;
                    if (op0 >= op1) doBranch(getOperand(2));
                }
                break;
            case 0x2c: // jgtu
                {
                    long op0 = getOperand(0) & 0x0ffffffffl;
                    long op1 = getOperand(1) & 0x0ffffffffl;
                    if (op0 > op1) doBranch(getOperand(2));
                }
                break;
            case 0x2d: // jleu
                {
                    long op0 = getOperand(0) & 0x0ffffffffl;
                    long op1 = getOperand(1) & 0x0ffffffffl;
                    if (op0 <= op1) doBranch(getOperand(2));
                }
                break;
            case 0x30: // call
                // Take the arguments from the stack and store them, prepareCall
                // will use them and store them according to the function type
                int funaddr = getOperand(0);
                _numArguments = getOperand(1);
                for (int i = 0; i < _numArguments; i++) {
                    _arguments[i] = state.popInt();
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
                    state.pushCallStub(_operands[0]);
                    storeAtOperand(0, state.sp); // catch token
                    doBranch(branchOffset);
                } else {
                    state.pushCallStub(_operands[0]);
                    storeAtOperand(0, state.sp); // catch token
                    doBranch(getOperand(1));
                }
                break;
            case 0x33: // throw
                int storeVal   = getOperand(0);
                int catchToken = getOperand(1);
                logger.info(String.format("@throw %d %d CURRENT SP = %d",
                                          storeVal, catchToken, state.sp));
                if (state.sp < catchToken)
                    throw new IllegalStateException("@throw: catch token > current SP !!!");
                state.sp = catchToken;
                logger.info(String.format("@throw, SP is now: %d\n", state.sp));
                state.popCallStubThrow(storeVal);
                logger.info(String.format("@throw, after popCallStub SP is now: %d\n",
                                          state.sp));
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
                storeAtOperand(1, Types.signExtend8(getOperand(0))); break;
            case 0x48: // aload
                {
                    int arr   = getOperand(0);
                    int index = getOperand(1);
                    storeAtOperand(2, state.memIntAt(arr + index * 4));
                }
                break;
            case 0x49: // aloads
                {
                    int arr = getOperand(0);
                    int index = getOperand(1);
                    storeAtOperand(2, state.memShortAt(arr + index * 2));
                }
                break;
            case 0x4a: // aloadb
                {
                    int arr    = getOperand(0);
                    int index  = getOperand(1);
                    storeAtOperand(2, state.memByteAt(arr + index));
                }
                break;
            case 0x4b: // aloadbit
                {
                    int addr      = getOperand(0);
                    int bitOffset = getOperand(1);
                    int memAddr   = addr + bitOffset / 8;
                    int bitnum    = bitOffset % 8;
                    if (bitnum < 0) {
                        // adjust bitnum if necessary
                        memAddr--;
                        bitnum += 8;
                    }
                    int mask = 1 << bitnum;
                    int test =
                        ((state.memByteAt(memAddr) & mask) == mask) ? 1 : 0;
                    storeAtOperand(2, test);
                }
                break;
            case 0x4c: // astore
                {
                    int arr   = getOperand(0);
                    int index = getOperand(1);
                    state.setMemIntAt(arr + index * 4, getOperand(2));
                }
                break;
            case 0x4d: // astores
                {
                    int arr   = getOperand(0);
                    int index = getOperand(1);
                    state.setMemShortAt(arr + index * 2, getOperand(2));
                }
                break;
            case 0x4e: // astoreb
                {
                    int arr   = getOperand(0);
                    int index = getOperand(1);
                    state.setMemByteAt(arr + index, getOperand(2));
                }
                break;
            case 0x4f: // astorebit
                {
                    int addr      = getOperand(0);
                    int bitOffset = getOperand(1);
                    int memAddr   = addr + bitOffset / 8;
                    int bitnum    = bitOffset % 8;
                    if (bitnum < 0) {
                        // adjust bitnum if necessary
                        memAddr--;
                        bitnum += 8;
                    }
                    if (getOperand(2) == 0) { // clear
                        state.setMemByteAt(memAddr,
                                           state.memByteAt(memAddr) & (~(1 << bitnum) & 0xff));
                    } else { // set
                        state.setMemByteAt(memAddr,
                                           state.memByteAt(memAddr) | ((1 << bitnum) & 0xff));
                    }
                }
                break;
            case 0x50: // stkcount
                storeAtOperand(0, state.numStackValuesInCallFrame()); break;
            case 0x51: // stkpeek
                storeAtOperand(1, state.stackPeek(getOperand(0))); break;
            case 0x52: // stkswap
                state.stackSwap(); break;
            case 0x53: // stkroll
                state.stackRoll(getOperand(0), getOperand(1)); break;
            case 0x54: // stkcopy
                {
                    int numElems  = getOperand(0);
                    int copyStart = state.sp - Types.SizeInt * numElems;
                    for (int i = 0; i < numElems; i++) {
                        state.pushInt(state.getIntInStack(copyStart + i * Types.SizeInt));
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
                        storeAtOperand(2, state.heapStart());
                    } else {
                        storeAtOperand(2, GlulxGestalt.gestalt(selector, arg));
                    }
                }
                break;
            case 0x101: // debugtrap
                fatal(String.format("[** ERROR, VM HALTED WITH CODE %d **]", getOperand(0))); break;
            case 0x102: // getmemsize
                storeAtOperand(0, state.memsize()); break;
            case 0x103: // setmemsize
                int newSize = getOperand(0);
                logger.info(String.format("@setmemsize %d\n", newSize));
                if (newSize < state.header.endmem()) fatal("@setmemsize: size must be >= ENDMEM");
                if (newSize % 256 != 0) fatal("@setmemsize: size must be multiple of 256");
                if (state.heapIsActive()) {
                    fatal("@setmemsize: can not set while heap is active");
                }
                state.setMemsize(newSize);
                // Result is 0 for success, 1 for fail
                break;
            case 0x104: // jumpabs
                state.pc = getOperand(0); break;
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
                state.pRunState = VMRunStates.Halted; break;
            case 0x121: // verify
                storeAtOperand(0, state.verify()); break;
            case 0x122: // restart
                restart(); break;
            case 0x123: // save
                {
                    int streamId = getOperand(0);
                    SaveGameWriter writer = new SaveGameWriter(glk, streamId, state, _operands[1]);
                    boolean result = writer.writeGameFile();
                    if (result) storeAtOperand(1, 0);
                    else storeAtOperand(1, 1);
                }
                break;
            case 0x124: // restore
                {
                    int streamId = getOperand(0);
                    SaveGameLoader loader = new SaveGameLoader(glk, streamId, state, _originalRam);
                    if (loader.loadGame()) {
                        state.popCallStubThrow(-1);
                    } else {
                        storeAtOperand(1, 1); // fail for now
                    }
                }
                break;
            case 0x125: // saveundo
                _undoSnapshots.add(state.createSnapshot(_operands[0]));
                storeAtOperand(0, 0); // Always say SUCCEED
                break;
            case 0x126: // restoreundo
                if (_undoSnapshots.size() > 0) {
                    state.readSnapshot(_undoSnapshots.get(_undoSnapshots.size() - 1),
                                       _protectionStart,
                                       _protectionLength);
                    _undoSnapshots.remove(_undoSnapshots.size() - 1);
                    state.popCallStubThrow(-1);
                } else {
                    storeAtOperand(0, 1); // fail
                }
                logger.info(String.format("RESTORED WITH PC: %02x AND FP: %d SP: %d",
                                          state.pc, state.fp, state.sp));
                break;
            case 0x127: // protect
                _protectionStart  = getOperand(0);
                _protectionLength = getOperand(1);
                break;
            case 0x130: // glk
                {
                    int glkId = getOperand(0);
                    int numArgs = getOperand(1);
                    int[] args = new int[numArgs];
                    for (int i = 0; i < numArgs; i++) {
                        args[i] = state.popInt();
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
                    int iosys = getOperand(0);
                    int rock  = getOperand(1);
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
                    int result = state.linearSearch.apply(getOperand(0), getOperand(1),
                                                          getOperand(2), getOperand(3),
                                                          getOperand(4), getOperand(5),
                                                          getOperand(6));
                    storeAtOperand(7, result);
                }
                break;
            case 0x151: // binarysearch
                {
                    int result = state.binarySearch.apply(getOperand(0), getOperand(1),
                                                          getOperand(2), getOperand(3),
                                                          getOperand(4), getOperand(5),
                                                          getOperand(6));
                    storeAtOperand(7, result);
                }
                break;
            case 0x152: // linkedsearch
                {
                    int result = state.linkedSearch.apply(getOperand(0), getOperand(1),
                                                          getOperand(2), getOperand(3),
                                                          getOperand(4), getOperand(5));
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
                state.mzero(getOperand(0), getOperand(1)); break;
            case 0x171: // mcopy
                state.mcopy(getOperand(0), getOperand(1), getOperand(2)); break;
            case 0x178: // malloc
                storeAtOperand(1, state.malloc(getOperand(0))); break;
            case 0x179: // mfree
                state.mfree(getOperand(0)); break;
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
        state.pRunState = VMRunStates.Halted;
    }
}
