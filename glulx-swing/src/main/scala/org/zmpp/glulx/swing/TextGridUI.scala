/*
 * Created on 2010/06/18
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
package org.zmpp.glulx.swing

import javax.swing._
import javax.swing.text.StyleConstants
import java.awt.Color
import java.awt.event._

import org.zmpp.base._
import org.zmpp.glk._

/**
 * UI representation of a text buffer. We use EditorPane, which allows for
 * styled text.
 */
class SwingTextGridUI(screenUI: SwingGlkScreenUI, glkWindow: GlkUIWindow)
extends SwingTextWindowUI(screenUI, glkWindow) {
  private var waitForMouse  = false
  private var _cursorx = 0
  private var _cursory = 0
  
  protected def numCols = getWidth / screenUI.charWidthTextGrid
  protected def numRows = getHeight / screenUI.lineHeightTextGrid

  setFont(screenUI.fixedFont)
  style = StyleType.Normal.id
  val attrs = getInputAttributes
  StyleConstants.setFontFamily(attrs, screenUI.fixedFont.getFamily)
  StyleConstants.setFontSize(attrs,   screenUI.fixedFont.getSize)  

  def reset = _clear
  def _clear {
    setBackground(new Color(currentBackgroundColor))
    setForeground(new Color(currentForegroundColor))
    _cursorx = 0
    _cursory = 0
    
    val text = new StringBuilder
    val nrows = numRows
    val ncols = numCols
    for (i <- 0 until nrows) {
      for (j <- 0 until ncols) {
        text.append(" ")
      }
      text.append("\n")
    }
    style = StyleType.Normal.id
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
  
  def currentPos = _cursory * numCols + _cursorx
  
  def _putChar(c: Char) {
    //printf("TextGrid.putChar('%c')\n", c)
    if (c == '\n') {
      _cursorx  = 0
      if (_cursory < numRows - 1) _cursory += 1
    } else {
      val index = currentPos
      // write to the right position in the document
      //_buffer.replace(index, index + 1, String.valueOf(c))
      val doc = getDocument
      doc.remove(index, 1)
      doc.insertString(index, String.valueOf(c), getInputAttributes)
      _cursorx += 1
      // wrap around if possible
      if (_cursorx >= numCols && _cursory < numRows) {
        _cursory += 1
        _cursorx = 0      
      }
      //setText(_buffer.toString)
    }
  }
  override def _setStyle(style: Int) {
    import StyleHintType._
    //logger.info("Setting TextGrid Style to: %s".format(StyleType(style).toString))
    flush
    val attrs = getInputAttributes
    val isBold = if (glkWindow.styleHints.get(style, Weight.id) == 1) true else false
    val isItalic = if (glkWindow.styleHints.get(style, Oblique.id) == 1) true else false
    val isReverse = if (glkWindow.styleHints.get(style, ReverseColor.id) == 1) true else false
    val backColor = if (isReverse) glkWindow.styleHints.get(style, TextColor.id)
      else glkWindow.styleHints.get(style, BackColor.id)
    val textColor = if (isReverse) glkWindow.styleHints.get(style, BackColor.id)
      else glkWindow.styleHints.get(style, TextColor.id)
    currentBackgroundColor = backColor
    currentForegroundColor = textColor
    StyleConstants.setBold(attrs, isBold)
    StyleConstants.setItalic(attrs, isItalic)
    StyleConstants.setBackground(attrs, new Color(backColor))
    StyleConstants.setForeground(attrs, new Color(textColor))
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
      logger.info("mouseClicked, POS = %d MAPPED TO X = %d Y = %d".format(pos, x, y))
      resumeWithMouseInput(x, y)
    }
  }

  def _setHyperlink(linkval: Int) {
    logger.info("SET HYPERLINK LINKVAL = " + linkval)
    if (linkval == 0) {
      flush
      // reset style to normal
      style = StyleType.Normal.id
      if (currentHyperLink != null) {
        currentHyperLink.endPos = currentPos
        hyperLinkMap(currentHyperLink.id) = currentHyperLink
        // This output can generate BadLocationExceptions !!!
        /*
        val doc = getDocument
        printf("ADDED HYPERLINK %d: start: %d end: %d text: '%s'\n",
          currentHyperLink.id, currentHyperLink.startPos, currentHyperLink.endPos,
          doc.getText(currentHyperLink.startPos, currentHyperLink.endPos))
          */
        currentHyperLink = null
      }
    } else {
      flush
      val attrs = getInputAttributes
      StyleConstants.setBold(attrs, false)
      StyleConstants.setItalic(attrs, false)
      StyleConstants.setUnderline(attrs, true)
      StyleConstants.setForeground(attrs, new Color(0x0000ff))
      currentHyperLink = new HyperLink(linkval)
      currentHyperLink.startPos = currentPos
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

