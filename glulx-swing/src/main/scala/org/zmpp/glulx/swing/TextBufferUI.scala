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
import javax.swing.text.StyledDocument

import java.awt.Color
import java.awt.Dimension
import java.awt.image.BufferedImage

import org.zmpp.glk._

/**
 * UI representation of a text buffer. We use EditorPane, which allows for
 * styled text.
 */
object SwingTextBufferUI {
  val MarginLeft   = 20
  val MarginRight  = 20
  val MarginTop    = 10
  val MarginBottom = 10
}
 
class SwingTextBufferUI(screenUI: SwingGlkScreenUI, glkWindow: GlkUIWindow)
extends SwingTextWindowUI(screenUI, glkWindow) {
  var buffer = new StringBuilder
  setMargin(new java.awt.Insets(SwingTextBufferUI.MarginTop,
                                SwingTextBufferUI.MarginLeft,
                                SwingTextBufferUI.MarginBottom,
                                SwingTextBufferUI.MarginRight))
  style = StyleType.Normal.id
  setStandardFont

  private def setStandardFont {
    val attrs = getInputAttributes
    setFont(screenUI.standardFont)
    StyleConstants.setFontFamily(attrs, screenUI.standardFont.getFamily)
    StyleConstants.setFontSize(attrs,   screenUI.standardFont.getSize)
  }
  private def setFixedFont {
    val attrs = getInputAttributes
    setFont(screenUI.fixedFont)
    StyleConstants.setFontFamily(attrs, screenUI.fixedFont.getFamily)
    StyleConstants.setFontSize(attrs,   screenUI.fixedFont.getSize)
  }

  protected def numCols = getWidth / screenUI.charWidthStdFont
  protected def numRows = getHeight / screenUI.lineHeightStdFont
  val container = new JScrollPane(this, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                  ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
  def _moveCursor(xpos: Int, ypos: Int) { }
  def _putChar(c: Char) {
    buffer.append(c)
    if (c == '\n') _flush
  }
  def _clear {
    _flush
    val newForeground = new Color(currentForegroundColor)
    setBackground(new Color(currentBackgroundColor))
    setForeground(newForeground)
    setCaretColor(newForeground)
    getDocument.remove(0, getDocument.getLength)
  }
  override def _flush {
    val attrs = getInputAttributes
    getDocument.insertString(getDocument.getLength, buffer.toString, attrs)
    buffer = new StringBuilder
  }
  override def _setStyle(style: Int) {
    import StyleHintType._
    flush
    val attrs = getInputAttributes
    val isProportional = if (glkWindow.styleHints.get(style, Proportional.id) == 1) true else false
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
    if (isProportional) setStandardFont
    else setFixedFont
  }
  override def setPreferredSize(size: Dimension) {
    super.setPreferredSize(size)
    container.setPreferredSize(size)
  }

  override def drawScaledImage(resnum: Int, posx: Int, posy: Int, width: Int, height: Int) {
    throw new UnsupportedOperationException("This window does not support drawing scaled images")
  }
  def _drawImage(resnum: Int, alignId: Int) {
    _flush
    if (alignId > 0 && alignId <= 5) {
      val align = ImageAlign(alignId)
    } else {
      logger.warning("INVALID ALIGNMENT ID: %d".format(alignId))
    }
    //logger.info("ALIGNMENT = %s\n".format(align.toString))
    val image = screenUI.getImage(resnum)
    val doc = getDocument.asInstanceOf[StyledDocument]
    val imgStyle = doc.addStyle("imgstyle", null)
    StyleConstants.setIcon(imgStyle, new ImageIcon(image))
    doc.insertString(doc.getLength, "imgtxt", imgStyle)
  }
  override def drawImage(resnum: Int, alignId: Int, ignore: Int) {
    if (SwingUtilities.isEventDispatchThread) _drawImage(resnum, alignId)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _drawImage(resnum, alignId)
      })
    }
  }
  def requestMouseInput {
    throw new UnsupportedOperationException("no mouse input in text buffers")
  }

  private def currentPos = getDocument.getLength

  def _setHyperlink(linkval: Int) {
    //throw new UnsupportedOperationException("no hyperlinks in text buffers")
    //    logger.info("SET HYPERLINK LINKVAL = " + linkval)
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
      SwingUtilities.invokeLater(new Runnable {
        def run = _putChar(c)
      })
    }
  }
  def putCharUni(c: Int) {
    if (SwingUtilities.isEventDispatchThread) _putChar(c.toChar)
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _putChar(c.toChar)
      })
    }
  }
}

