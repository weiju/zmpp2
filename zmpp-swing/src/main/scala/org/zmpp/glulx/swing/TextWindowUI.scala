/*
 * Created on 2010/06/18
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
package org.zmpp.glulx.swing

import java.util.logging._
import javax.swing._
import javax.swing.text.StyleConstants
import javax.swing.text.StyledDocument
import java.awt.Color
import java.awt.Cursor
import java.awt.Point
import java.awt.event._

import scala.collection.mutable.HashMap

import org.zmpp.base._
import org.zmpp.glk._

object SwingTextWindowUI {
  val InputModeNone = 0
  val InputModeLine = 1
  val InputModeChar = 2
}
abstract class SwingTextWindowUI(val screenUI: SwingGlkScreenUI,
                                 val glkWindow: GlkUIWindow)
extends JTextPane with SwingGlkWindowUI with KeyListener {
  val logger = Logger.getLogger("glk.ui")
  var textInputMode = SwingTextWindowUI.InputModeNone
  var listenToHyperlinkEvents = false
  var inputStart = 0
  var currentBackgroundColor = 0x00ffffff
  var currentForegroundColor = 0x00000000

  // a map of hyperlinks, cleared when the screen is cleared
  val hyperlinkMap = new HashMap[Int, Hyperlink]
  var currentHyperlink: Hyperlink = null
  def isHyperlinkMode = currentHyperlink != null
  
  addKeyListener(this)
  addMouseListener(this)
  addMouseMotionListener(this)

  protected def numCols    : Int
  protected def numRows    : Int
  protected def currentPos : Int
  
  def eventManager = screenUI.vm.eventManager
  
  def setBackgroundColor(color: Int) = currentBackgroundColor = color

  /** Always return a smaller width to Glk to avoid formatting issues */
  def glkSize = new GlkDimension(numCols - 1, numRows)
  def glkSize_=(size: GlkDimension) {
    throw new UnsupportedOperationException(
      "Setting text buffer window size not supported")
  }
  
  def isLineInputMode = textInputMode == SwingTextWindowUI.InputModeLine
  def isCharInputMode = textInputMode == SwingTextWindowUI.InputModeChar
  
  protected def stopSendingRequests {
    textInputMode = SwingTextWindowUI.InputModeNone
    listenToHyperlinkEvents = false
  }

  private def resumeExecution {
    if (screenUI.vm.state.runState == VMRunStates.WaitForEvent &&
        eventManager.processNextEvent) {
      style = StyleType.Normal
      stopSendingRequests
      ExecutionControl.executeTurn(screenUI.vm)   
    }
  }

  protected def resumeWithLineInput(input: String) {
    logger.info("RESUME WITH LINE INPUT, WINDOW: %d".format(glkWindow.id))
    eventManager.addLineInputEvent(glkWindow.id, input)
    resumeExecution
  }

  protected def resumeWithCharInput(charCode: Int) {
    logger.info("RESUME WITH CHAR INPUT, WINDOW: %d".format(glkWindow.id))
    eventManager.addCharInputEvent(glkWindow.id, charCode)
    resumeExecution
  }
  
  protected def resumeWithHyperlink(id: Int) {
    eventManager.addHyperlinkEvent(glkWindow.id, id)
    resumeExecution
  }

  def keyPressed(event: KeyEvent) {
    if (isCharInputMode) {
      val keyCode = glkKeyCode(event.getKeyCode)
      if (keyCode != GlkKeyCodes.Unknown) {
        event.consume
        resumeWithCharInput(keyCode)
      }
    } else if (isLineInputMode) {
      val doc = getDocument
      val caretPos = getCaret.getDot
      if (caretPos < inputStart) getCaret.setDot(doc.getLength)

      if (event.getKeyCode == KeyEvent.VK_ENTER) {
        event.consume
        val input = doc.getText(inputStart, doc.getLength - inputStart)
        logger.info("Input was: '%s'".format(input))
        putChar('\n')
        resumeWithLineInput(input)
      } else if (event.getKeyCode == KeyEvent.VK_BACK_SPACE ||
                 event.getKeyCode == KeyEvent.VK_LEFT) {
        if (getCaret.getDot <= inputStart) {
          event.consume
        }
        // make sure that input style is not "removed".
        // Note: This sometimes works and sometimes not, why is that ?
        style = StyleType.Input
      } else if (event.getKeyCode == KeyEvent.VK_UP) {
        event.consume
      }
    } else {
      // no input mode, eat the key event
      event.consume
    }
  }
  def keyTyped(event: KeyEvent) {
    if (isCharInputMode) {
      event.consume
      resumeWithCharInput(event.getKeyChar.toInt)
    } else if (!isLineInputMode) {
      // not in input mode, eat all key events
      event.consume
    }
  }
  def keyReleased(event: KeyEvent) {}
  
  // has to be called in UI event thread
  def requestLineInput {
    style = StyleType.Input
    requestFocusInWindow
    getCaret.setVisible(true)
    inputStart = getDocument.getLength
    textInputMode = SwingTextWindowUI.InputModeLine
  }
  def requestPreviousLineInput {
    style = StyleType.Input
    requestFocusInWindow
    getCaret.setVisible(true)
    // here we assume the input mark is the one from the last select
    textInputMode = SwingTextWindowUI.InputModeLine
  }
  def requestCharInput {
    requestFocusInWindow
    getCaret.setVisible(true)
    textInputMode = SwingTextWindowUI.InputModeChar
  }
  def requestHyperlinkEvent {
    listenToHyperlinkEvents = true
  }
  
  var _incompleteInput: String = null
  def _cancelLineInput {
    val doc = getDocument
    _incompleteInput = doc.getText(inputStart, doc.getLength - inputStart)
    logger.info("CANCELLED WITH LINE INPUT: '%s'\n".format(_incompleteInput))
  }

  def cancelLineInput: String = {
    if (SwingUtilities.isEventDispatchThread) _cancelLineInput
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _cancelLineInput
      })
    }
    _incompleteInput
  }

  def _setHyperlink(linkval: Int) {
    if (linkval == 0) {
      flush
      if (currentHyperlink != null) {
        currentHyperlink.endPos = currentPos
        hyperlinkMap(currentHyperlink.id) = currentHyperlink
        currentHyperlink = null
      }
      // reset style to normal, we have to set this AFTER setting the
      // link to NULL (the order is important) !!
      style = StyleType.Normal
    } else {
      flush
      val attrs = getInputAttributes
      StyleConstants.setBold(attrs, false)
      StyleConstants.setItalic(attrs, false)
      StyleConstants.setUnderline(attrs, true)
      StyleConstants.setForeground(attrs, new Color(0x0000ff))
      StyleConstants.setBackground(attrs, new Color(currentBackgroundColor))
      currentHyperlink = new Hyperlink(linkval)
      currentHyperlink.startPos = currentPos
    }
  }

  
  def setHyperlink(linkval: Int) {
    if (SwingUtilities.isEventDispatchThread) _setHyperlink(linkval)
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _setHyperlink(linkval)
      })
    }
  }

  private def glkKeyCode(keyCode: Int): Int = keyCode match {
    case KeyEvent.VK_LEFT      => GlkKeyCodes.Left
    case KeyEvent.VK_RIGHT     => GlkKeyCodes.Right
    case KeyEvent.VK_UP        => GlkKeyCodes.Up
    case KeyEvent.VK_DOWN      => GlkKeyCodes.Down
    case KeyEvent.VK_ENTER     => GlkKeyCodes.Return
    case KeyEvent.VK_DELETE    => GlkKeyCodes.Delete
    case KeyEvent.VK_ESCAPE    => GlkKeyCodes.Escape
    case KeyEvent.VK_TAB       => GlkKeyCodes.Tab
    case KeyEvent.VK_PAGE_UP   => GlkKeyCodes.PageUp
    case KeyEvent.VK_PAGE_DOWN => GlkKeyCodes.PageDown
    case KeyEvent.VK_HOME      => GlkKeyCodes.Home
    case KeyEvent.VK_END       => GlkKeyCodes.End
    case KeyEvent.VK_F1        => GlkKeyCodes.Func1
    case KeyEvent.VK_F2        => GlkKeyCodes.Func2
    case KeyEvent.VK_F3        => GlkKeyCodes.Func3
    case KeyEvent.VK_F4        => GlkKeyCodes.Func4
    case KeyEvent.VK_F5        => GlkKeyCodes.Func5
    case KeyEvent.VK_F6        => GlkKeyCodes.Func6
    case KeyEvent.VK_F7        => GlkKeyCodes.Func7
    case KeyEvent.VK_F8        => GlkKeyCodes.Func8
    case KeyEvent.VK_F9        => GlkKeyCodes.Func9
    case KeyEvent.VK_F10       => GlkKeyCodes.Func10
    case KeyEvent.VK_F11       => GlkKeyCodes.Func11
    case KeyEvent.VK_F12       => GlkKeyCodes.Func12
    case _                     => GlkKeyCodes.Unknown
  }

  private def hyperlinkAtPos(pos: Int): Hyperlink = {
    for (hyperlink <- hyperlinkMap.values) {
      if (hyperlink.contains(pos)) return hyperlink
    }
    null
  }

  def mouseMoved(event: MouseEvent) {
    val pos = viewToModel(event.getPoint)
    if (hyperlinkAtPos(pos) != null) {
      setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
    } else {
      setCursor(Cursor.getDefaultCursor)
    }
  }
  def mouseDragged(event: MouseEvent) { }
  def mouseExited(event: MouseEvent) { }
  def mouseEntered(event: MouseEvent) { }
  def mouseReleased(event: MouseEvent) { }
  def mousePressed(event: MouseEvent) { }
  def mouseClicked(event: MouseEvent) {
    if (listenToHyperlinkEvents) {
      val pos = viewToModel(event.getPoint)
      val hyperlink = hyperlinkAtPos(pos)
      if (hyperlink != null) {
        resumeWithHyperlink(hyperlink.id)
      }
    }
  }
}

