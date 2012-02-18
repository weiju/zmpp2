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

class AccelFuncEntry {
    public int callAddress;
    public AccelFunc func;

    public AccelFuncEntry(int callAddress, AccelFunc func) {
        this.callAddress = callAddress;
        this.func = func;
    }
}

class AccelSystem {
    public static int MaxAccelParams = 9;

    private GlulxVM vm;
    private static Logger logger = Logger.getLogger("glulx.accel");
    private int[] _accelParams = new int[MaxAccelParams];
    private AccelFuncEntry[] _accelFunctions = new AccelFuncEntry[100];
    private int _numAccelFuncs;
    public  Glk glk;

    public AccelSystem(GlulxVM vm) {
        this.vm = vm;
    }

    private AccelFunc accelFuncFor(int funcnum) {
        switch (funcnum) {
        case 1: return new Func1ZRegion(vm, glk, _accelParams);
        case 2: return new Func2CPTab(vm, glk, _accelParams);
        case 3: return new Func3RAPr(vm, glk, _accelParams);
        case 4: return new Func4RLPr(vm, glk, _accelParams);
        case 5: return new Func5OCCl(vm, glk, _accelParams);
        case 6: return new Func6RVPr(vm, glk, _accelParams);
        case 7: return new Func7OPPr(vm, glk, _accelParams);
        default:
            logger.warning(String.format("Unsupported Function number: %d", funcnum));
            return null;
        }
    }

    private AccelFunc accelFuncForCallAddress(int callAddress) {
        for (int i = 0; i < _numAccelFuncs; i++) {
            if (_accelFunctions[i].callAddress == callAddress) return _accelFunctions[i].func;
            i++;
        }
        return null;
    }

    private void removeFuncForCallAddress(int callAddress) {
        int i = 0;
        boolean found = false;
        while (!found && i < _numAccelFuncs) {
            if (_accelFunctions[i].callAddress == callAddress) {
                found = true;
            } else i++;
        }
        // now shift everything from i to the left to fill the gap
        while (i < _numAccelFuncs - 1) {
            _accelFunctions[i] = _accelFunctions[i + 1];
            i++;
        }
        _numAccelFuncs--;
    }

    private void addFuncForCallAddress(int callAddress, AccelFunc func) {
        _accelFunctions[_numAccelFuncs++] = new AccelFuncEntry(callAddress, func);
    }

    public void setParameter(int index, int value) {
        if (index >= 0 && index <= 8) _accelParams[index] = value;
    }

    public void setFunction(int accelFuncNum, int callAddress) {
        //logger.info("accelfunc #$%02x #$%02x".format(accelFuncNum, callAddress));
        if (accelFuncNum == 0) removeFuncForCallAddress(callAddress);
        else {
            AccelFunc accelFunc = accelFuncFor(accelFuncNum);
            if (accelFunc != null) addFuncForCallAddress(callAddress, accelFunc);
        }
    }

    public boolean isAccelerated(int callAddress) {
        return _numAccelFuncs > 0 && accelFuncForCallAddress(callAddress) != null;
    }

    public void call(int callAddress, int[] args, int numArgs) {
        //logger.info("Function address $%02x is accelerated - REPLACE (TODO)")
        AccelFunc func = accelFuncForCallAddress(callAddress);
        int retval = func.call(args, numArgs);
        vm.popCallStub(retval);
    }
}
