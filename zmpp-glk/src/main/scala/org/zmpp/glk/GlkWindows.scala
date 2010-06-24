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

import java.util.logging._

import org.zmpp.iff._

/**
 * Pure data holder to contain layout information for windows.
 */
class GlkLayoutTreeNode(_id: Int) {
  def id = _id
  var parent: GlkLayoutTreeNode = null
  var child0: GlkLayoutTreeNode = null
  var child1: GlkLayoutTreeNode = null
  var size   = 0
  var method = 0

  def isLeaf = (child0 == null)
  var isGraphics = false
  def position = (method & GlkWindowPosition.Mask)
  def division = (method & GlkWindowDivision.Mask)

  def isProportional = division == GlkWindowDivision.Proportional
  def isFixed = division == GlkWindowDivision.Fixed
  def isVertical = isAbove || isBelow
  def isHorizontal = isLeft || isRight
  def isLeft = position == GlkWindowPosition.Left
  def isRight = position == GlkWindowPosition.Right
  def isAbove = position == GlkWindowPosition.Above
  def isBelow = position == GlkWindowPosition.Below
}

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
  def updateLayout(root: GlkLayoutTreeNode)
  def createTextBufferUI(id: Int, glkWindow: GlkUIWindow): GlkWindowUI
  def createTextGridUI(id: Int,   glkWindow: GlkUIWindow): GlkWindowUI
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

  def pollEvents: GlkEventType.Value
  def cancelLineInput(windowId: Int): String
}

/**
 * Base class of Glk windows to implement the Composite pattern.
 */
abstract class GlkWindow(val id: Int, var size: Int, val rock: Int) {
  var ui: GlkWindowUI = null
  var parent: GlkWindow = null
  def outputStream : GlkStream
  def typeName: String
  def wintype: Int
}

/**
 * Pair windows only exist in the Glk windowing system. Since they only act as
 * inner nodes of the layout tree they do not have an UI equivalent.
 */
class GlkPairWindow(id: Int) extends GlkWindow(id, 0, 0) {
  var child0: GlkWindow = null
  var child1: GlkWindow = null // key window child
  var outputStream = NilStream
  var method = 0
  def wintype = GlkWindowType.PairWindow.id
  def typeName = "Pair"
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

  val outputStream = new GlkStream {
    var id = 0
    def rock = 0
    def close { }
    def readCount = {
      throw new UnsupportedOperationException("MemoryOutputStream8 does not support readCount")
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
    }
    def putCharUni(c: Int) {
      ui.putCharUni(c)
      _writeCount += 1
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
  def wintype = GlkWindowType.Graphics.id
  def styleHints = null
  def typeName = "Graphics"
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
    logger.info("CREATED WINDOW WITH ID %d TYPE: %s SIZE: %d".format(id, wintype.toString, size))

    import GlkWindowType._
    val newWindow: GlkWindow = wintype match {
      case TextBuffer =>
        val window = new GlkUIWindow(id, size, rock) {
          def wintype = GlkWindowType.TextBuffer.id
          def styleHints = _textbufferStyleHints
          def typeName = "TextBuffer"
        }
        window.ui = screenUI.createTextBufferUI(id, window)
        window
      case TextGrid   =>
        val window = new GlkUIWindow(id, size, rock) {
          def wintype = GlkWindowType.TextGrid.id
          def styleHints = _textgridStyleHints
          def typeName = "TextGrid"
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
  def imageSize(resnum: Int): GlkDimension = screenUI.imageSize(resnum)
  def fillRect(winId: Int, color: Int, left: Int, top: Int, width: Int, height: Int) {
    windowWithId(winId).ui.fillRect(color, left, top, width, height)
  }
  def setBackgroundColor(winId: Int, color: Int) {
    windowWithId(winId).ui.setBackgroundColor(color)
  }
  def getParent(winId: Int): Int = {
    if (winId == _rootWindow.id) 0
    else windowWithId(winId).parent.id
  }
  def getRock(winId: Int): Int = windowWithId(winId).rock
  def getSize(winId: Int): GlkDimension = windowWithId(winId).ui.glkSize
  def getType(winId: Int) = windowWithId(winId).wintype
  def moveCursor(winId: Int, xpos: Int, ypos: Int) {
    windowWithId(winId).ui.moveCursor(xpos, ypos)
  }  
  def open(split: Int, method: Int, size: Int, wintype: Int, rock: Int): Int = {
    val newWindow = createWindow(GlkWindowType(wintype), size, rock)
    if (split > 0) {
      splitWindow(windowWithId(split), newWindow, method)
    }
    screenUI.updateLayout(createLayoutTree)
    newWindow.id
  }
  def closeWindow(winId: Int): Int = {
    val windowToClose = windowWithId(winId)
    val writeCount = windowToClose.outputStream.writeCount
    
    val winParentId = if (windowToClose.parent == null) -1 else windowToClose.parent.id
    logger.info("CLOSING WINDOW WITH ID: %d (PARENT ID: %d)".format(winId, winParentId))

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
    screenUI.updateLayout(createLayoutTree)
    writeCount
  }
  def setArrangement(winId: Int, method: Int, size: Int, keywinId: Int) {
    val pair = windowWithId(winId).asInstanceOf[GlkPairWindow]
    val keyWindow = if (keywinId == 0) pair.child1 else windowWithId(keywinId)
    if (keyWindow != pair.child1) {
      throw new IllegalArgumentException("key window is not key child of specified pair !")
    } else {
      pair.method = method
      pair.child1.size = size
      //logger.info("update window arrangement")
      screenUI.updateLayout(createLayoutTree)
    }
  }
  
  def outputStreamForWindow(winId: Int) = windowWithId(winId).outputStream

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

  /**
   * Creates a tree containing only layout information of the current
   * window hierarchy.
   */
  private def createLayoutTree: GlkLayoutTreeNode = {
    createLayoutTree(_rootWindow)
  }
  
  /**
   * Recursively copy layout information from the window tree into a layout
   * tree.
   */
  private def createLayoutTree(window: GlkWindow): GlkLayoutTreeNode = {
    if (window == null) return null
    logger.info("REC CREATE LAYOUT TREE - WINDOW TYPE: " + window.typeName + " id = " + window.id)
    val node = new GlkLayoutTreeNode(window.id)
    node.size = window.size
    if (window.isInstanceOf[GlkGraphicsUIWindow]) node.isGraphics = true
    if (window.isInstanceOf[GlkPairWindow]) {
      val pair = window.asInstanceOf[GlkPairWindow]
      node.method = pair.method
      node.child0 = createLayoutTree(pair.child0)
      node.child1 = createLayoutTree(pair.child1)
      if (node.child0 != null) node.child0.parent = node
      if (node.child1 != null) node.child1.parent = node
    }
    node
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
      newParent.child0   = tosplit
      newParent.child1   = newWindow
      newParent.method   = method
      newParent.child0.parent = newParent
      newParent.child1.parent = newParent
      if (_rootWindow == newParent.child0) _rootWindow = newParent
      logger.info("SPLITTING WINDOW WITH ID: %d SIZE: %d POS: %s DIV: %s PAIR PARENT: %d".format(
             tosplit.id, newWindow.size, GlkWindowPosition.name(method),
             GlkWindowDivision.name(method), if (oldParent == null) -1 else oldParent.id))
  }
}

