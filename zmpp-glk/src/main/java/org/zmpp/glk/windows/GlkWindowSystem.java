/*
 * Created on 2012/02/22
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
package org.zmpp.glk.windows;

import java.util.logging.*;
import java.util.List;
import java.util.ArrayList;

import org.zmpp.base.*;
import org.zmpp.iff.*;
import org.zmpp.glk.io.*;
import org.zmpp.glk.styles.*;
import org.zmpp.glk.*;

/**
 * The window system. Mainly a facade that manages the windows both in
 * a map for quick id-based access and a tree to store the render hierarchy.
 */
public class GlkWindowSystem {
    private static Logger logger = Logger.getLogger("glk");
    private GlkIOSystem _ioSystem;
    private int _nextId = 1;
    private StyleHints _textgridStyleHints   = new StyleHints();
    private StyleHints _textbufferStyleHints = new StyleHints();

    // root of the window tree
    private GlkWindow _rootWindow;

    // quickly access windows by id
    private List<GlkWindow> _windows = new ArrayList<GlkWindow>();
    public GlkScreenUI screenUI;

    public void reset(GlkIOSystem iosys) {
        _ioSystem = iosys;
        _nextId = 1;
        _windows.clear();
        _textgridStyleHints.reset();
        _textbufferStyleHints.reset();
    }

    public GlkWindow iterate(int winId) {
        if (_windows.isEmpty()) return null;
        else if (winId == 0) return _windows.get(0);
        else {
            int i = 0;
            for (;i < _windows.size(); i++) {
                if (_windows.get(i).id == winId) break;
            }
            return (i < _windows.size() - 2) ? _windows.get(i + 1) : null;
        }
    }

    public void clearStyleHint(int wintype, int styleNum, int hintNum) {
        switch (wintype) {
        case GlkWindowType.TextBuffer:
            _textbufferStyleHints.reset(styleNum, hintNum);
            break;
        case GlkWindowType.TextGrid:
            _textgridStyleHints.reset(styleNum, hintNum);
            break;
        case GlkWindowType.All:
            _textbufferStyleHints.reset(styleNum, hintNum);
            _textgridStyleHints.reset(styleNum, hintNum);
            break;
        default:
            break;
        }
    }

    public void setStyleHint(int wintype, int styleNum, int hintNum,
                             int value) {
        switch (wintype) {
        case GlkWindowType.TextBuffer:
            _textbufferStyleHints.set(styleNum, hintNum, value);
            break;
        case GlkWindowType.TextGrid:
            _textgridStyleHints.set(styleNum, hintNum, value);
            break;
        case GlkWindowType.All:
            _textbufferStyleHints.set(styleNum, hintNum, value);
            _textgridStyleHints.set(styleNum, hintNum, value);
            break;
        default:
            break;
        }
    }

    public int styleMeasure(VMState state, int winId, int style, int hint,
                            int resultPtr) {
        GlkWindow window = windowWithId(winId);
        if (window != null) {
            switch (window.wintype()) {
            case GlkWindowType.TextBuffer:
                {
                    int value = _textbufferStyleHints.get(style, hint);
                    if (resultPtr != 0) state.setMemIntAt(resultPtr, value);
                    return 1;
                }
            case GlkWindowType.TextGrid:
                {
                    int value = _textgridStyleHints.get(style, hint);
                    if (resultPtr != 0) state.setMemIntAt(resultPtr, value);
                    return 1;
                }
            default:
                break;
            }
        }
        return 0;
    }

    public int styleDistinguish(int winId, int style1, int style2) {
        GlkWindow window = windowWithId(winId);
        if (window != null) {
            switch (window.wintype()) {
            case GlkWindowType.TextBuffer:
                return _textbufferStyleHints.distinguishable(style1, style2) ? 1 : 0;
            case GlkWindowType.TextGrid:
                return _textgridStyleHints.distinguishable(style1, style2) ? 1 : 0;
            default:
                break;
            }
        }
        return 0;
    }


    public int rootWindowId() { return (_rootWindow == null) ? 0 : _rootWindow.id; }
    public void clearWindow(int winId) {
        //logger.info("clearWindow(), winId: %d".format(winId))
        GlkWindow win = windowWithId(winId);
        if (win != null) win.ui.clear();
    }

    public GlkWindow createWindow(int wintype, int size, int rock) {
        //logger.info("createWindow type: %s size: %d rock: %d".format(wintype, size,
        //                                                             rock))
        int id = _nextId++;

        GlkWindow newWindow = null;
        switch (wintype) {
        case GlkWindowType.TextBuffer:
            newWindow = new GlkUIWindow(id, size, rock) {
                    public int wintype() { return GlkWindowType.TextBuffer; }
                    public StyleHints styleHints() { return _textbufferStyleHints; }
                    public String typeName() { return "TextBuffer"; }
                    public boolean isTextBuffer() { return true; }
                    public boolean isTextGrid() { return false; }
                };
            newWindow.ui = screenUI.createTextBufferUI(id, (GlkUIWindow) newWindow);
            break;
        case GlkWindowType.TextGrid:
            newWindow = new GlkUIWindow(id, size, rock) {
                    public int wintype() { return GlkWindowType.TextGrid; }
                    public StyleHints styleHints() { return _textgridStyleHints; }
                    public String typeName() { return "TextGrid"; }
                    public boolean isTextBuffer() { return false; }
                    public boolean isTextGrid() { return true; }
                };
            newWindow.ui = screenUI.createTextGridUI(id, (GlkUIWindow) newWindow);
            break;
        case GlkWindowType.Graphics:
            newWindow = new GlkGraphicsUIWindow(id, size, rock);
            newWindow.ui = screenUI.createGraphicsUI(id, newWindow);
            break;
        case GlkWindowType.PairWindow:
            newWindow = new GlkPairWindow(id);
            break;
        default:
            throw new IllegalArgumentException(String.format("unknown window type: %d\n", wintype));
        }
        _ioSystem.registerStream(newWindow.outputStream());
        _windows.add(newWindow);
        if (_rootWindow == null) _rootWindow = newWindow;
        return newWindow;
    }

    public int drawImage(int winId, int resnum, int posx, int posy) {
        windowWithId(winId).ui.drawImage(resnum, posx, posy);
        return 1;
    }

    public int drawScaledImage(int winId, int resnum, int posx, int posy,
                               int width, int height) {
        windowWithId(winId).ui.drawScaledImage(resnum, posx, posy, width, height);
        return 1;
    }

    public void eraseRect(int winId, int left, int top, int width, int height) {
        windowWithId(winId).ui.eraseRect(left, top, width, height);
    }

    public void fillRect(int winId, int color, int left, int top, int width, int height) {
        windowWithId(winId).ui.fillRect(color, left, top, width, height);
    }

    public GlkDimension imageSize(int resnum) { return screenUI.imageSize(resnum); }

    public void setBackgroundColor(int winId, int color) {
        //logger.info("setBackgroundColor() win: %d color: %02x".format(winId, color))
        windowWithId(winId).ui.setBackgroundColor(color);
    }

    public int getParent(int winId) {
        return (winId == _rootWindow.id) ? 0 : windowWithId(winId).parent.id;
    }

    public int getRock(int winId) { return windowWithId(winId).rock; }

    public int getSibling(int winId) {
        if (winId == _rootWindow.id) return 0;
        else {
            GlkWindow refWindow = windowWithId(winId);
            GlkPairWindow parent = (GlkPairWindow) refWindow.parent;
            return (refWindow == parent.child0) ? parent.child1.id : parent.child0.id;
        }
    }

    public GlkDimension getSize(int winId) {
        GlkWindow window = windowWithId(winId);
        return (window != null) ? window.ui.glkSize() : new GlkDimension(0, 0);
    }

    public int getStreamId(int winId) { return windowWithId(winId).outputStream().id(); }
    public int getType(int winId) { return windowWithId(winId).wintype(); }
    public void moveCursor(int winId, int xpos, int ypos) {
        windowWithId(winId).ui.moveCursor(xpos, ypos);
    }

    public int open(int split, int method, int size, int wintype, int rock) {
        GlkWindow newWindow = createWindow(wintype, size, rock);
        //logger.info("open(), split: %d method: %d, size: %d, wintype: %d, rock: %d, ID = %d".format(split, method, size, wintype, rock, newWindow.id))
        if (split > 0) {
            splitWindow(windowWithId(split), newWindow, method);
        }
        screenUI.updateLayout(_rootWindow);
        return newWindow.id;
    }

    public int closeWindow(int winId) {
        //logger.info("closeWindow(), winId: %d".format(winId))
        GlkWindow windowToClose = windowWithId(winId);
        if (windowToClose == null) return 0;
        int writeCount = windowToClose.outputStream().writeCount();
        int winParentId = (windowToClose.parent == null) ? -1 : windowToClose.parent.id;

        // remove window from its parent by replacing its parent with the sibling
        if (windowToClose.parent != null) {
            GlkPairWindow winParent = (GlkPairWindow) windowToClose.parent;
            GlkWindow sibling = (windowToClose == winParent.child0) ? winParent.child1 : winParent.child0;
            if (winParent == _rootWindow) {
                sibling.parent = null;
                _rootWindow = sibling;
            } else {
                GlkPairWindow winGrandParent = (GlkPairWindow) winParent.parent;
                sibling.parent = winGrandParent;
                if (winParent == winGrandParent.child0) winGrandParent.child0 = sibling;
                else winParent.child1 = sibling;
            }
            windowToClose.parent = null;
        }
        if (windowToClose == _rootWindow) _rootWindow = null;
        screenUI.updateLayout(_rootWindow);
        return writeCount;
    }
  
    public void setArrangement(int winId, int method, int size, int keywinId) {
        GlkPairWindow pair = (GlkPairWindow) windowWithId(winId);
        GlkWindow keyWindow = (keywinId == 0) ? pair.keyWindow : windowWithId(keywinId);
        if (keyWindow != pair.keyWindow) {
            throw new IllegalArgumentException("keyWindow is not key window of specified pair !");
        } else {
            pair.method         = method;
            pair.keyWindow.size = size;
            screenUI.updateLayout(_rootWindow);
        }
    }

    public void getArrangement(VMState state, int winId, int methodPtr, int sizePtr,
                               int keyWinPtr) {
        GlkPairWindow pair = (GlkPairWindow) windowWithId(winId);
        state.setMemIntAt(methodPtr, pair.method);
        state.setMemIntAt(sizePtr, pair.keyWindow.size);
        state.setMemIntAt(keyWinPtr, pair.keyWindow.id);
    }

    public void flowBreak(int winId) {
        logger.info(String.format("WINDOW.FLOW_BREAK, WINDOW = %d (NOT IMPLEMENTED)", winId));
    }
  
    public GlkStream outputStreamForWindow(int winId) { return windowWithId(winId).outputStream(); }
    public int getEchoStream(int winId) {
        GlkWindow window = windowWithId(winId);
        if (window == null) return 0;
        else if (window.echoStream() == null) return 0;
        else return window.echoStream().id();
    }

    public void setEchoStream(int winId, int streamId) {
        windowWithId(winId).setEchoStream(_ioSystem.streamWithId(streamId));
    }

    // *********************************************************************
    // ***** Private methods
    // ****************************
    
    private GlkWindow windowWithId(int id) {
        for (GlkWindow window : _windows) {
            if (window.id == id) return window;
        }
        logger.warning(String.format("Window #%d not found in window list !!", id));
        return null;
    }

    private void splitWindow(GlkWindow tosplit, GlkWindow newWindow, int method) {
        GlkPairWindow oldParent = (GlkPairWindow) tosplit.parent;
        GlkPairWindow newParent = (GlkPairWindow)
            createWindow(GlkWindowType.PairWindow, 0, 0);
        // parent of tosplit becomes newParent's parent
        newParent.parent   = oldParent;
        if (oldParent != null) {
            if (tosplit == oldParent.child0) oldParent.child0 = newParent;
            else oldParent.child1 = newParent;
        }
        newParent.child0        = tosplit;
        newParent.child1        = newWindow;
        newParent.keyWindow     = newWindow;
        newParent.method        = method;
        newParent.child0.parent = newParent;
        newParent.child1.parent = newParent;
        if (_rootWindow == newParent.child0) _rootWindow = newParent;
    }
}
