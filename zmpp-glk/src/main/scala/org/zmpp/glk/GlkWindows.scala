/*
 * Created on 2010/04/09
 * Copyright (c) 2010, Wei-ju Wu.
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
package org.zmpp.glk

import java.io.File
import java.util.logging._

import org.zmpp.iff._

/**
 * Interface to be implemented by the user interface, technology-dependent.
 * These are to access the representations of the visual window objects.
 */
trait GlkWindowUI {
  def glkSize: GlkDimension
  def glkSize_=(size: GlkDimension)
  def style: Int
  def style_=(style: Int)
  def moveCursor(xpos: Int, ypos: Int)
  def putChar(c: Char)
  def putCharUni(c: Int)
  def eraseRect(left: Int, top: Int, width: Int, height: Int)
  def fillRect(color: Int, left: Int, top: Int, width: Int, height: Int)
  def drawScaledImage(resnum: Int, posx: Int, posy: Int, width: Int, height: Int)
  def drawImage(resnum: Int, posx: Int, posy: Int)
  def flush
  def clear
  def setBackgroundColor(color: Int)
  def setHyperlink(linkval: Int)
}

/**
 * Interface to be implemented by the user interface, technology-dependent.
 * Represents the screen as a whole.
 */
trait GlkScreenUI {
  def imageSize(imageNum: Int): GlkDimension
  def updateLayout(root: GlkWindow)
  def createTextBufferUI(id: Int, glkUiWindow: GlkUIWindow): GlkWindowUI
  def createTextGridUI(id: Int,   glkUiWindow: GlkUIWindow): GlkWindowUI
  def createGraphicsUI(id: Int,   glkWindow: GlkWindow): GlkWindowUI
  
  def requestLineInput(windowId: Int)

  /*
   * A small trick that works with the current ZMPP event model: If
   * the line input was suspended (e.g. by a timer input or something)
   * calling this method let's us pick up at the previous input mark.
   * requestLineInput() would simply start at wherever the cursor is
   * at the moment, while this method picks up where it was when the
   * pending line input was requested at the first time.
   */
  def requestPreviousLineInput(windowId: Int)
  def requestCharInput(windowId: Int)
  def requestMouseInput(windowId: Int)  
  def requestTimerInput(millis: Int)
  def requestHyperlinkEvent(windowId: Int)

  def cancelLineInput(windowId: Int): String
  
  /*
   * Asks the user interface to have the user select a file. Returns null if
   * cancelled, otherwise the full path to the file.
   */
  def selectFileByDialog(usage: Int, fmode: Int): File
}

/**
 * Base class of Glk windows to implement the Composite pattern.
 */
abstract class GlkWindow(val id: Int, var size: Int, val rock: Int) {
  var ui           : GlkWindowUI = null
  var parent       : GlkWindow = null
  def outputStream : GlkStream
  def echoStream   : GlkStream
  def echoStream_=(stream: GlkStream)
  def typeName     : String
  def wintype      : Int
  def isGraphics   : Boolean = false
  def isLeaf       : Boolean = true
  def isTextBuffer : Boolean
  def isTextGrid   : Boolean
}

/**
 * Pair windows only exist in the Glk windowing system. Since they only act as
 * inner nodes of the layout tree they do not have an UI equivalent.
 */
class GlkPairWindow(id: Int) extends GlkWindow(id, 0, 0) {
  var child0       : GlkWindow = null
  var child1       : GlkWindow = null
  var keyWindow    : GlkWindow = null// key window child
  var outputStream = NilStream
  var method       = 0
  def wintype      = GlkWindowType.PairWindow.id
  def typeName     = "Pair"
  def isTextBuffer = false
  def isTextGrid   = false
  def echoStream   = null
  def echoStream_=(stream: GlkStream) = {
    throw new UnsupportedOperationException("Can not attach echo stream to pair window")
  }
 
  override def isLeaf = false
  def position = (method & GlkWindowPosition.Mask)
  def division = (method & GlkWindowDivision.Mask)
  def isProportional = division == GlkWindowDivision.Proportional
  def isFixed        = division == GlkWindowDivision.Fixed
  def isLeft         = position == GlkWindowPosition.Left
  def isRight        = position == GlkWindowPosition.Right
  def isAbove        = position == GlkWindowPosition.Above
  def isBelow        = position == GlkWindowPosition.Below
  def isVertical     = isAbove || isBelow
  def isHorizontal   = isLeft || isRight
}

/**
 * A user interface with an UI representation which it delegates to.
 * These can be of type blank, text buffer, text grid and graphics.
 */
abstract class GlkUIWindow(id: Int, size: Int, rock: Int)
extends GlkWindow(id, size, rock) {
  val logger = Logger.getLogger("glk.ui")
  private var _style = 0
  private var _writeCount = 0
  private var _buffer = new StringBuilder
  def styleHints: StyleHints

  var echoStream: GlkStream = null
  val outputStream = new GlkStream {
    var id = 0
    def rock = 0
    def close { }
    def readCount = {
      throw new UnsupportedOperationException("WindowStream does not support readCount")
    }
    def getChar = {
      throw new UnsupportedOperationException("WindowStream does not support getChar")
    }
    def getCharUni = {
      throw new UnsupportedOperationException("WindowStream does not support getCharUni")
    }
    def writeCount = _writeCount
    def position   = 0 // TODO
    def style = _style
    def style_=(value: Int) = {
      _style = value
      ui.style = value
    }
    def putChar(c: Char) {
      ui.putChar(c)
      _writeCount += 1
      
      // write to echo stream
      if (echoStream != null) echoStream.putChar(c)
    }
    def putCharUni(c: Int) {
      ui.putCharUni(c)
      _writeCount += 1
      
      // write to echo stream
      if (echoStream != null) echoStream.putCharUni(c)
    }
    def seek(newpos: Int, seekmode: Int) {
      throw new UnsupportedOperationException("WindowStream.seek() not supported")
    }
    def setHyperlink(linkval: Int) = ui.setHyperlink(linkval)
  }
}

class GlkGraphicsUIWindow(id: Int, size: Int, rock: Int)
extends GlkWindow(id, size, rock) {
  var outputStream = NilStream
  def wintype      = GlkWindowType.Graphics.id
  def styleHints   = null
  def typeName     = "Graphics"
  override def isGraphics = true
  def isTextBuffer = false
  def isTextGrid   = false

  def echoStream   = null
  def echoStream_=(stream: GlkStream) = {
    throw new UnsupportedOperationException("Can not attach echo stream to pair window")
  }
}

/**
 * The window system. Mainly a facade that manages the windows both in
 * a map for quick id-based access and a tree to store the render hierarchy.
 */
class GlkWindowSystem {
  val logger = Logger.getLogger("glk")
  private var _ioSystem : GlkIOSystem = null
  private var _nextId = 1
  private val _textgridStyleHints   = new StyleHints
  private val _textbufferStyleHints = new StyleHints

  // root of the window tree
  private var _rootWindow: GlkWindow = null

  // quickly access windows by id
  private var _windows : List[GlkWindow] = Nil
  var screenUI: GlkScreenUI = null

  def reset(iosys: GlkIOSystem) {
    _ioSystem = iosys
    _nextId = 1
    _windows = Nil
    _textgridStyleHints.reset
    _textbufferStyleHints.reset
  }
  def iterate(winId: Int): GlkWindow = {
    if (_windows.isEmpty) null
    else if (winId == 0) _windows.head
    else {
      val remain = _windows.dropWhile(window => window.id != winId).tail
      if (remain.isEmpty) null
      else remain.head 
    }
  }
  def clearStyleHint(wintype: GlkWindowType.Value, styleNum: Int, hintNum: Int) {
    wintype match {
      case GlkWindowType.TextBuffer =>
        _textbufferStyleHints.reset(styleNum, hintNum)
      case GlkWindowType.TextGrid   =>
        _textgridStyleHints.reset(styleNum, hintNum)
      case GlkWindowType.All        =>
        _textbufferStyleHints.reset(styleNum, hintNum)
        _textgridStyleHints.reset(styleNum, hintNum)
      case _ => // do nothing
    }
  }
  def setStyleHint(wintype: GlkWindowType.Value, styleNum: Int, hintNum: Int, value: Int) {
    wintype match {
      case GlkWindowType.TextBuffer =>
        _textbufferStyleHints.set(styleNum, hintNum, value)
      case GlkWindowType.TextGrid   =>
        _textgridStyleHints.set(styleNum, hintNum, value)
      case GlkWindowType.All        =>
        _textbufferStyleHints.set(styleNum, hintNum, value)
        _textgridStyleHints.set(styleNum, hintNum, value)
      case _ => // do nothing
    }
  }

  def rootWindowId = if (_rootWindow == null) 0 else _rootWindow.id
  def clearWindow(winId: Int) = windowWithId(winId).ui.clear
  def createWindow(wintype: GlkWindowType.Value, size: Int, rock: Int) = {
    val id = nextId

    import GlkWindowType._
    val newWindow: GlkWindow = wintype match {
      case TextBuffer =>
        val window = new GlkUIWindow(id, size, rock) {
          def wintype = GlkWindowType.TextBuffer.id
          def styleHints = _textbufferStyleHints
          def typeName = "TextBuffer"
          def isTextBuffer = true
          def isTextGrid   = false
        }
        window.ui = screenUI.createTextBufferUI(id, window)
        window
      case TextGrid   =>
        val window = new GlkUIWindow(id, size, rock) {
          def wintype = GlkWindowType.TextGrid.id
          def styleHints   = _textgridStyleHints
          def typeName     = "TextGrid"
          def isTextBuffer = false
          def isTextGrid   = true
        }
        window.ui = screenUI.createTextGridUI(id, window)
        window
      case Graphics   =>
        val window = new GlkGraphicsUIWindow(id, size, rock)
        window.ui = screenUI.createGraphicsUI(id, window)
        window
      case PairWindow =>
        new GlkPairWindow(id)
      case _ =>
        throw new IllegalArgumentException("unknown window type: %s\n".format(wintype.toString))
    }
    _ioSystem.registerStream(newWindow.outputStream)
    _windows ::= newWindow
    if (_rootWindow == null) _rootWindow = newWindow
    newWindow
  }
  def drawImage(winId: Int, resnum: Int, posx: Int, posy: Int): Int = {
    windowWithId(winId).ui.drawImage(resnum, posx, posy)
    1
  }
  def drawScaledImage(winId: Int, resnum: Int, posx: Int, posy: Int,
                      width: Int, height: Int): Int = {
    windowWithId(winId).ui.drawScaledImage(resnum, posx, posy, width, height)
    1
  }
  def eraseRect(winId: Int, left: Int, top: Int, width: Int, height: Int) {
    windowWithId(winId).ui.eraseRect(left, top, width, height)
  }
  def fillRect(winId: Int, color: Int, left: Int, top: Int, width: Int, height: Int) {
    windowWithId(winId).ui.fillRect(color, left, top, width, height)
  }
  def imageSize(resnum: Int): GlkDimension = screenUI.imageSize(resnum)
  def setBackgroundColor(winId: Int, color: Int) {
    windowWithId(winId).ui.setBackgroundColor(color)
  }
  def getParent(winId: Int): Int = {
    if (winId == _rootWindow.id) 0
    else windowWithId(winId).parent.id
  }
  def getRock(winId: Int)     = windowWithId(winId).rock
  def getSibling(winId: Int): Int  = {
    if (winId == _rootWindow.id) 0
    else {
      val refWindow = windowWithId(winId)
      val parent = refWindow.parent.asInstanceOf[GlkPairWindow]
      if (refWindow == parent.child0) parent.child1.id
      else parent.child0.id
    }
  }
  def getSize(winId: Int)     = windowWithId(winId).ui.glkSize
  def getStreamId(winId: Int) = windowWithId(winId).outputStream.id
  def getType(winId: Int)     = windowWithId(winId).wintype
  def moveCursor(winId: Int, xpos: Int, ypos: Int) {
    windowWithId(winId).ui.moveCursor(xpos, ypos)
  }
  def open(split: Int, method: Int, size: Int, wintype: Int, rock: Int): Int = {
    val newWindow = createWindow(GlkWindowType(wintype), size, rock)
    if (split > 0) {
      splitWindow(windowWithId(split), newWindow, method)
    }
    screenUI.updateLayout(_rootWindow)
    newWindow.id
  }
  def closeWindow(winId: Int): Int = {
    val windowToClose = windowWithId(winId)
    val writeCount = windowToClose.outputStream.writeCount
    
    val winParentId = if (windowToClose.parent == null) -1 else windowToClose.parent.id

    // remove window from its parent by replacing its parent with the sibling
    if (windowToClose.parent != null) {
      val winParent = windowToClose.parent.asInstanceOf[GlkPairWindow]
      val sibling =
        if (windowToClose == winParent.child0) winParent.child1
        else winParent.child0
      if (winParent == _rootWindow) {
        sibling.parent = null
        _rootWindow = sibling
      } else {
        val winGrandParent = winParent.parent.asInstanceOf[GlkPairWindow]
        sibling.parent = winGrandParent
        if (winParent == winGrandParent.child0) winGrandParent.child0 = sibling
        else winParent.child1 == sibling
      }
      windowToClose.parent = null
    }
    if (windowToClose == _rootWindow) _rootWindow = null
    screenUI.updateLayout(_rootWindow)
    writeCount
  }
  
  def setArrangement(winId: Int, method: Int, size: Int, keywinId: Int) {
    val pair = windowWithId(winId).asInstanceOf[GlkPairWindow]
    val keyWindow = if (keywinId == 0) pair.keyWindow else windowWithId(keywinId)
    if (keyWindow != pair.keyWindow) {
      throw new IllegalArgumentException("keyWindow is not key window of specified pair !")
    } else {
      pair.method         = method
      pair.keyWindow.size = size
      screenUI.updateLayout(_rootWindow)
    }
  }
  
  def outputStreamForWindow(winId: Int) = windowWithId(winId).outputStream
  def getEchoStream(winId: Int): Int = {
    val window = windowWithId(winId)
    if (window == null) 0
    else if (window.echoStream == null) 0
    else window.echoStream.id
  }
  def setEchoStream(winId: Int, streamId: Int) {
    windowWithId(winId).echoStream = _ioSystem.streamWithId(streamId)
  }

  // *********************************************************************
  // ***** Private methods
  // ****************************
  
  private def nextId = {
    _nextId += 1
    _nextId - 1
  }
  
  private def windowWithId(id: Int) = {
    _windows.filter(window => window.id == id).head
  }
  
  private def splitWindow(tosplit: GlkWindow, newWindow: GlkWindow,
                          method: Int) {
      val oldParent = tosplit.parent.asInstanceOf[GlkPairWindow]
      val newParent =
        createWindow(GlkWindowType.PairWindow, 0, 0).asInstanceOf[GlkPairWindow]
      // parent of tosplit becomes newParent's parent
      newParent.parent   = oldParent
      if (oldParent != null) {
        if (tosplit == oldParent.child0) oldParent.child0 = newParent
        else oldParent.child1 = newParent
      }
      newParent.child0        = tosplit
      newParent.child1        = newWindow
      newParent.keyWindow     = newWindow
      newParent.method        = method
      newParent.child0.parent = newParent
      newParent.child1.parent = newParent
      if (_rootWindow == newParent.child0) _rootWindow = newParent
  }
}

