/*
 * Created on 2011/12/10
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
package org.zmpp.zcode;

/**
 * The object table is basically an updatable view into the Z-Machine's memory.
 * The concept separating into V1-V3 and V4-V8 object tables is kept from
 * ZMPP 1.x (this is actually just the Scala port of the ZMPP 1.x object table)
 */
public abstract class ObjectTable {
    
    protected VMState _vmState;
    protected int objectTableAddress;

    public ObjectTable(VMState state) {
        _vmState = state;
        objectTableAddress = _vmState.header().objectTable();
    }

    public void removeObject(int obj) {
        int oldParent = parent(obj);
        setParent(obj, 0);
        if (oldParent != 0) {
            if (child(oldParent) == obj) {
                // removed object was first child, set sibling as the first child now
                setChild(oldParent, sibling(obj));
            } else {
                // removed object was not the first child
                // find the previous sibling in the chain and set the removed object's
                // next sibling as the previous sibling's next sibling
                int currentChild = child(oldParent);
                int currentSibling = sibling(currentChild);
        
                while (currentSibling != 0 && currentSibling != obj) {
                    currentChild   = currentSibling;
                    currentSibling = sibling(currentChild);
                }
                // sibling might be 0, in that case, the object is not
                // in the hierarchy
                if (currentSibling == obj) {
                    setSibling(currentChild, sibling(obj));
                }
            }
        }
        setSibling(obj, 0);
    }

    public void insertObject(int obj, int dest) {
        if (parent(obj) > 0) removeObject(obj);
        int oldChild = child(dest);
        setParent(obj, dest);
        setChild(dest, obj);
        setSibling(obj, oldChild);
    }
    protected abstract int parent(int obj);
    protected abstract void setParent(int obj, int newParent);
    protected abstract int child(int obj);
    protected abstract void setChild(int obj, int newChild);
    protected abstract int sibling(int obj);
    protected abstract void setSibling(int obj, int newSibling);

    public boolean isAttributeSet(int obj, int attr) {
        int value = _vmState.byteAt(attributeAddress(obj, attr));
        return (value & (0x80 >> (attr & 7))) > 0;
    }

    public void setAttribute(int obj, int attr) {
        if (obj > 0) { 
            int attrAddress = attributeAddress(obj, attr);
            int value = _vmState.byteAt(attrAddress);
            _vmState.setByteAt(attrAddress, value | (0x80 >> (attr & 7)));
        }
    }

    public void clearAttribute(int obj, int attr) {
        if (obj > 0) {
            int attrAddress = attributeAddress(obj, attr);
            int value = _vmState.byteAt(attrAddress);
            _vmState.setByteAt(attrAddress, value & ~(0x80 >> (attr & 7)));
        }
    }

    public int propertyTableAddress(int obj) {
        return _vmState.shortAt(objectAddress(obj) + objectEntrySize() - 2);
    }

    public int propertyValue(int obj, int prop) {
        int propAddr = propertyAddress(obj, prop);
        if (propAddr == 0) return propertyDefault(prop);
        else {
            if (propertyLength(propAddr) == 1) return _vmState.byteAt(propAddr);
            // 2 is assumed if longer, we just write two bytes
            else return _vmState.shortAt(propAddr);
        }
    }

    public void setPropertyValue(int obj, int prop, int value)
        throws PropertyDoesNotExistException {
        final int propAddr = propertyAddress(obj, prop);
        if (propAddr == 0) throw new PropertyDoesNotExistException();
        else {
            if (propertyLength(propAddr) == 1) {
                _vmState.setByteAt(propAddr, value & 0xff);
            } else {
                _vmState.setShortAt(propAddr, value & 0xffff);
            }
        }
    }

    public int propertyAddress(int obj, int prop) {
        int propAddr = propertyEntriesStart(obj);
        while (true) {
            int propnum = propertyNum(propAddr);
            if (propnum == 0) return 0;
            int numPropSizeBytes = numPropertySizeBytes(propAddr);
            if (propnum == prop) return propAddr + numPropSizeBytes;
            propAddr += numPropSizeBytes + propertyLength(propAddr + numPropSizeBytes);
        }
    }

    public int nextProperty(int obj, int prop) throws PropertyDoesNotExistException {
        if (prop == 0) return propertyNum(propertyEntriesStart(obj));
        else {
            final int propDataAddr = propertyAddress(obj, prop);
            if (propDataAddr == 0) {
                throw new PropertyDoesNotExistException();
            } else {
                return propertyNum(propDataAddr + propertyLength(propDataAddr));
            }
        }
    }

    // Protected members
    protected int objectTreeStart() {
        return objectTableAddress + propertyDefaultTableSize();
    }

    protected int objectAddress(int obj) {
        return objectTreeStart() + (obj - 1) * objectEntrySize();
    }

    // abstract members
    public abstract int propertyLength(int propDataAddr);
    protected abstract int propertyDefaultTableSize();
    protected abstract int objectEntrySize();
    protected abstract int propertyNum(int propAddr);
    protected abstract int numPropertySizeBytes(int propAddr);
  
    // Private members
    private int attributeAddress(int obj, int attr) {
        return objectAddress(obj) + attr / 8;
    }

    private int propertyDefault(int prop) {
        return _vmState.shortAt(objectTableAddress + ((prop - 1) << 1));
    }
    private int propertyEntriesStart(int obj) {
        int propTableAddr = propertyTableAddress(obj);
        return propTableAddr + (_vmState.byteAt(propTableAddr) << 1) + 1;
    }
}

class ClassicObjectTable extends ObjectTable {
    public ClassicObjectTable(VMState vmState) {
        super(vmState);
    }
  
    protected int parent(int obj) { return  _vmState.byteAt(objectAddress(obj) + 4); }
    protected void setParent(int obj, int newParent) {
        _vmState.setByteAt(objectAddress(obj) + 4, newParent);
    }
    protected int sibling(int obj) { return _vmState.byteAt(objectAddress(obj) + 5); }
    protected void setSibling(int obj, int newSibling) {
        _vmState.setByteAt(objectAddress(obj) + 5, newSibling);
    }
    protected int child(int obj) { return _vmState.byteAt(objectAddress(obj) + 6); }
    protected void setChild(int obj, int newChild) {
        _vmState.setByteAt(objectAddress(obj) + 6, newChild);
    }

    protected int propertyDefaultTableSize() { return 31 * 2; }
    protected int objectEntrySize() { return 9; }

    protected int propertyNum(int propAddr) {
        return _vmState.byteAt(propAddr) - 32 * (propertyLength(propAddr + 1) - 1);
    }
    public int propertyLength(int propDataAddr) {
        if (propDataAddr == 0) return 0; // Note: defined in Z-Machine Standard 1.1
        else {
            // The size byte is always the byte before the property data in any
            // version, so this is consistent
            return _vmState.byteAt(propDataAddr - 1) / 32 + 1;
        }
    }
    protected int numPropertySizeBytes(int propAddr) { return 1; }
}

class ModernObjectTable extends ObjectTable {

    public ModernObjectTable(VMState vmState) { super(vmState); }

    protected int parent(int obj) { return _vmState.shortAt(objectAddress(obj) + 6); }
    protected void setParent(int obj, int newParent) {
        _vmState.setShortAt(objectAddress(obj) + 6, newParent);
    }
    protected int sibling(int obj) { return _vmState.shortAt(objectAddress(obj) + 8); }
    protected void setSibling(int obj, int newSibling) {
        _vmState.setShortAt(objectAddress(obj) + 8, newSibling);
    }
    protected int child(int obj) {
        return _vmState.shortAt(objectAddress(obj) + 10);
    }
    protected void setChild(int obj, int newChild) {
        _vmState.setShortAt(objectAddress(obj) + 10, newChild);
    }

    protected int propertyDefaultTableSize() { return  63 * 2; }
    protected int objectEntrySize() { return 14; }
    protected int propertyNum(int propAddr) { return _vmState.byteAt(propAddr) & 0x3f; }
    public int propertyLength(int propDataAddr) {
        if (propDataAddr == 0) return 0; // Z-Machine Standard 1.1
        else {
            int sizeByte = _vmState.byteAt(propDataAddr - 1);
            if ((sizeByte & 0x80) == 0x80) {
                int proplen = sizeByte & 0x3f;
                return (proplen == 0) ? 64 : proplen; // Standard 1.0 4.2.1.1
            } else {
                return ((sizeByte & 0x40) == 0x40) ? 2 : 1;
            }
        }
    }
    protected int numPropertySizeBytes(int propAddr) {
        return ((_vmState.byteAt(propAddr) & 0x80) == 0x80) ? 2 : 1;
    }
}
