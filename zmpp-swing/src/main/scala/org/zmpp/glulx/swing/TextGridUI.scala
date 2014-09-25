/*
 * Created on 2010/06/18
 * Copyright (c) 2010-2014, Wei-ju Wu.
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

import javax.swing._
import javax.swing.text.StyleConstants
import java.awt.Color
import java.awt.event._

import org.zmpp.base._
import org.zmpp.glk._

/**
 * UI representation of a text grid. We use EditorPane, which allows for
 * styled text.
 */
object SwingTextGridUI {
  val MarginLeft   = 5
  val MarginRight  = 5
  val MarginTop    = 3
  val MarginBottom = 3
}

class SwingTextGridUI(screenUI: SwingGlkScreenUI, glkWindow: GlkUIWindow)
extends SwingTextWindowUI(screenUI, glkWindow) {

  setMargin(new java.awt.Insets(SwingTextGridUI.MarginTop,
                                SwingTextGridUI.MarginLeft,
                                SwingTextGridUI.MarginBottom,
                                SwingTextGridUI.MarginRight))

  setFont(screenUI.fixedFont)
  style = StyleType.Normal
  val attrs = getInputAttributes
  StyleConstants.setFontFamily(attrs, screenUI.fixedFont.getFamily)
  StyleConstants.setFontSize(attrs,   screenUI.fixedFont.getSize)  

  private[this] var waitForMouse  = false
  private[this] var _cursorx = 0
  private[this] var _cursory = 0
  
  protected def numCols =
    (getWidth - SwingTextGridUI.MarginLeft - SwingTextGridUI.MarginRight) /
    screenUI.charWidthTextGrid
  protected def numRows =
    (getHeight - SwingTextGridUI.MarginTop - SwingTextGridUI.MarginBottom) /
    screenUI.lineHeightTextGrid

  def reset = _clear
  def _clear {
    setBackground(new Color(currentBackgroundColor))
    setForeground(new Color(currentForegroundColor))
    _cursorx = 0
    _cursory = 0
    
    val text = new StringBuilder
    val nrows = numRows
    val ncols = numCols
    var i = 0
    var j = 0
    while (i < nrows) {
      j = 0
      while (j < ncols) {
        text.append(" ")
        j += 1
      }
      text.append("\n")
      i += 1
    }
    style = StyleType.Normal
    setText("")
    getDocument.insertString(0, text.toString, getInputAttributes)
  }

  def container = this
  override protected def resumeWithLineInput(input: String) {
    super.resumeWithLineInput(input)
    waitForMouse = false
  }
  override protected def resumeWithCharInput(charCode: Int) {
    super.resumeWithCharInput(charCode)
    waitForMouse = false
  }
  
  private def resumeWithMouseInput(xpos: Int, ypos: Int) {
    eventManager.addMouseEvent(glkWindow.id, xpos, ypos)
    textInputMode = SwingTextWindowUI.InputModeNone
    waitForMouse = false
    if (screenUI.vm.state.runState == VMRunStates.WaitForEvent &&
      eventManager.processNextEvent) {
      ExecutionControl.executeTurn(screenUI.vm)   
    }
  }


  def _moveCursor(xpos: Int, ypos: Int) {
    _cursorx = xpos
    _cursory = ypos
  }
  
  protected def currentPos = _cursory * numCols + _cursorx
  
  def _putChar(c: Char) {
    if (c == '\n') {
      _cursorx  = 0
      if (_cursory < numRows - 1) _cursory += 1
    } else {
      val index = currentPos
      // write to the right position in the document
      val doc = getDocument
      doc.remove(index, 1)
      doc.insertString(index, String.valueOf(c), getInputAttributes)
      _cursorx += 1
      // wrap around if possible
      if (_cursorx >= numCols && _cursory < numRows) {
        _cursory += 1
        _cursorx = 0      
      }
    }
  }
  override def _setStyle(style: Int) {
    if (isHyperlinkMode) return // ignore style in hyperlink mode
    import StyleHintType._
    flush
    val attrs = getInputAttributes
    val isBold =
      if (glkWindow.styleHints.get(style, Weight) == 1) true else false
    val isItalic =
      if (glkWindow.styleHints.get(style, Oblique) == 1) true else false

    val isReverse =
      if (glkWindow.styleHints.get(style, ReverseColor) == 1) true else false
    var backColor = glkWindow.styleHints.get(style, BackColor)
    var textColor = glkWindow.styleHints.get(style, TextColor)

    if (backColor >= 0) {
      currentBackgroundColor = backColor
    } else {
      backColor = currentBackgroundColor
    }
    if (textColor >= 0) {
      currentForegroundColor = textColor
    } else {
      textColor = currentForegroundColor
    }
    if (isReverse) {
      StyleConstants.setForeground(attrs, new Color(backColor))
      StyleConstants.setBackground(attrs, new Color(textColor))
    } else {
      StyleConstants.setForeground(attrs, new Color(textColor))
      StyleConstants.setBackground(attrs, new Color(backColor))
    }
    StyleConstants.setBold(attrs, isBold)
    StyleConstants.setItalic(attrs, isItalic)
    StyleConstants.setUnderline(attrs, false)
  }
  def requestMouseInput {
    waitForMouse = true
  }
  override def mouseClicked(event: MouseEvent) {
    if (waitForMouse) {
      // map mouse coordinates to character pos
      val pos = viewToModel(new java.awt.Point(event.getPoint))
      val y = pos / numCols
      val x = pos % numCols
      logger.info("mouseClicked, POS = %d MAPPED TO X = %d Y = %d".format(pos,
                                                                          x, y))
      resumeWithMouseInput(x, y)
    }
  }

  def putChar(c: Char) {
    if (SwingUtilities.isEventDispatchThread) _putChar(c)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _putChar(c)
      })
    }
  }
  def putCharUni(c: Int) {
    if (SwingUtilities.isEventDispatchThread) _putChar(c.toChar)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _putChar(c.toChar)
      })
    }
  }

}

