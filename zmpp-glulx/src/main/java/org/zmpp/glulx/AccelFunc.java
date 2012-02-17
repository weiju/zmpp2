/*
 * Created on 2012/02/16
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
import org.zmpp.glk.Glk;

abstract class AccelFunc {

    protected static Logger logger = Logger.getLogger("glulx.accel");
    protected static final int numZRegion = 1;
    protected static final int numCpTab   = 1;
    protected static final int numRaPr    = 1;
    protected static final int numRlPr    = 1;

    protected GlulxVMState _state;
    private   Glk          _glk;
    private   int[]        _accelParams;

    public AccelFunc(GlulxVMState state, Glk glk,
                     int[] accelParams) {
        this._state = state;
        this._glk = glk;
        this._accelParams = accelParams;
    }

    protected int argIfGiven(int[] args, int numArgs, int index) {
        return (index >= numArgs) ? 0 : args[index];
    }

    protected abstract int call(int[] args, int numArgs);

    // Define functions for inner reuse
    // func 1
    protected int zRegion(int addr) {
        if (addr < 36) return 0;
        if (addr >= _state.memsize()) return 0;

        int tb = _state.memByteAt(addr);
        if      (tb >= 0xe0) return 3;
        else if (tb >= 0xc0) return 2;
        else if (tb >= 0x70 && tb <= 0x7f && addr >= _state.header.ramstart()) return 1;
        else return 0;
    }

    // func 2
    protected int cpTab(int obj, int id) {
        if (zRegion(obj) != 1) {
            accelError("[** Programming error: tried to find the \".\" of (something) **]");
            return 0;
        }
        int otab = _state.memIntAt(obj + 16);
        if (otab == 0) return 0;
        int max = _state.memIntAt(otab);
        return _state.binarySearch.apply(id, 2, otab + 4, 10, max, 0, 0);
    }

    // func 3
    protected int raPr(int obj, int id) {
        int prop = getProp(obj, id);
        return (prop == 0) ? 0 : _state.memIntAt(prop + 4);
    }

    // func 4
    protected int rlPr(int obj, int id) {
        int prop = getProp(obj, id);
        return (prop == 0) ? 0 : 4 * _state.memShortAt(prop + 2);
    }

    // func 5
    protected int ocCl(int obj, int cla) {
        int zr = zRegion(obj);

        if (zr == 3) return (cla == StringMetaclass()) ? 1 : 0;
        if (zr == 2) return (cla == RoutineMetaclass()) ? 1 : 0;
        if (zr != 1) return 0;
    
        if (cla == ClassMetaclass()) {
            if (objInClass(obj))           return 1;
            if (obj == ClassMetaclass())   return 1;
            if (obj == StringMetaclass())  return 1;
            if (obj == RoutineMetaclass()) return 1;
            if (obj == ObjectMetaclass())  return 1;
            return 0;
        }
        if (cla == ObjectMetaclass()) {
            if (objInClass(obj))           return 0;
            if (obj == ClassMetaclass())   return 0;
            if (obj == StringMetaclass())  return 0;
            if (obj == RoutineMetaclass()) return 0;
            if (obj == ObjectMetaclass())  return 0;
            return 1;
        }
    if ((cla == StringMetaclass()) || (cla == RoutineMetaclass())) return 0;
    
        if (!objInClass(cla)) {
            accelError("[** Programming error: tried to apply 'ofclass' with non-class **]");
            return 0;
        }
        int prop = getProp(obj, 2);
        if (prop == 0) return 0;
        int inlist = _state.memIntAt(prop + 4);
        if (inlist == 0) return 0;
        int inlistlen = _state.memShortAt(prop + 2);

        for (int j = 0; j < inlistlen; j++) {
            if (_state.memIntAt(inlist + 4 * j) == cla) return 1;
        }
        return 0;
    }

    // Helpers
    protected int ClassesTable()     { return _accelParams[0]; }
    protected int IndivPropStart()   { return _accelParams[1]; }
    protected int ClassMetaclass()   { return _accelParams[2]; }
    protected int ObjectMetaclass()  { return _accelParams[3]; }
    protected int RoutineMetaclass() { return _accelParams[4]; }
    protected int StringMetaclass()  { return _accelParams[5]; }
    protected int Self()             { return _accelParams[6]; }
    protected int NumAttrBytes()     { return _accelParams[7]; }
    protected int CpvStart()         { return _accelParams[8]; }
    protected int Call()             { return IndivPropStart() + 5; }
    protected int Print()            { return IndivPropStart() + 6; }
    protected int PrintToArray()     { return IndivPropStart() + 7; }

    public void accelError(String msg) {
        _glk.put_char('\n');
        _glk.put_java_string(msg);
        _glk.put_char('\n');
    }

    public boolean objInClass(int obj) {
        return _state.memIntAt(obj + 13 + NumAttrBytes()) == ClassMetaclass();
    }

    protected int getProp(int anObject, int anId) {
        int obj = anObject;
        int id  = anId;
        int cla = 0;

        if ((id & 0xffff0000) != 0) {
            cla = _state.memIntAt(ClassesTable() + (id & 0xffff) * 4);
            if (ocCl(obj, cla) == 0) return 0;
            id >>= 16;
            obj = cla;
        }

        int prop = cpTab(obj, id);
        if (prop == 0) return 0;

        if (objInClass(obj) && cla == 0) {
            if (id < IndivPropStart() || id >= IndivPropStart() + 8) return 0;
        }
        if (_state.memIntAt(Self()) != obj) {
            if ((_state.memByteAt(prop + 9) & 1) != 0) return 0;
        }
        return prop;
    }  
}

class Func1ZRegion extends AccelFunc {
    public Func1ZRegion(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }
    public int call(int[] args, int numArgs) {
        return (numArgs < 1) ? 0 : zRegion(args[0]);
    }
}

class Func2CPTab extends AccelFunc {
    public Func2CPTab(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }
    public int call(int[] args, int numArgs) {
        return cpTab(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1));
    }
}

class Func3RAPr extends AccelFunc {
    public Func3RAPr(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }
    public int call(int[] args, int numArgs) {
        return raPr(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1));
    }
}

class Func4RLPr extends AccelFunc {
    public Func4RLPr(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }
    public int call(int[] args, int numArgs) {
        return rlPr(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1));
    }
}

class Func5OCCl extends AccelFunc {
    public Func5OCCl(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }

    public int call(int[] args, int numArgs) {
        return ocCl(argIfGiven(args, numArgs, 0), argIfGiven(args, numArgs, 1));
    }
}

class Func6RVPr extends AccelFunc {
    public Func6RVPr(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }

    public int call(int[] args, int numArgs) {
        int id = argIfGiven(args, numArgs, 1);
        int addr = raPr(args[0], id);
        if (addr == 0) {
            if (id > 0 && id < IndivPropStart()) {
                return _state.memIntAt(CpvStart() + 4 * id);
            } else {
                accelError("[** Programming error: tried to read (something) **]");
                return 0;
            }
        } else return _state.memIntAt(addr);
    }
}

class Func7OPPr extends AccelFunc {
    public Func7OPPr(GlulxVMState state, Glk glk, int[] accelParams) {
        super(state, glk, accelParams);
    }

    public int call(int[] args, int numArgs) {
        int obj = argIfGiven(args, numArgs, 0);
        int id  = argIfGiven(args, numArgs, 1);
        int zr = zRegion(obj);
        if (zr == 3) {
            return (id == Print() || id == PrintToArray()) ? 1 : 0;
        }
        if (zr == 2) {
            return (id == Call()) ? 1 : 0;
        }
        if (zr != 1) return 0;
        if (id >= IndivPropStart() && id < IndivPropStart() + 8) {
            if (objInClass(obj)) return 1;
        }
        return (raPr(obj, id) == 0) ? 0 : 1;
    }
}
