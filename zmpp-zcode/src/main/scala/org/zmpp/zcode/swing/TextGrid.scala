/*
 * Created on 2010/08/07
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
package org.zmpp.zcode.swing

import javax.swing._
import java.awt._
import java.awt.event._

import org.zmpp.zcode.{ScreenModelWindow, TextGridBuffer, TextStyles}

// A class to implement the top window. In Swing, the top window sits
// in the glass pane. This is done to implement the tricky behaviour
// of the Z-machine screen model of overlaying bottom window in a
// possibly overlapping way.
// The standard layout containers either don't overlap or do not
// show the stacked components below. This is also one of the reasons
// why the Glk can't fully implement the Z-machine screen model, V6
// is a another story.
class TextGrid extends ScreenModelWindow {

  var textPane = new JTextPane
  private var numLines     = 0
  private var _cursorPos   = (1, 1)
  // Even though the text grid is a non-buffered window type, we still buffer
  // it up to prepare for resizing of the main window
  var buffer: TextGridBuffer = null
  var totalLines   = 0
  var charsPerLine = 0

  // we need access to the screen model to access
  var screenModel    : SwingScreenModel = null

  textPane.setOpaque(false)

  def addFocusListener(listener: FocusListener) { textPane.addFocusListener(listener) }
  def setFont(font: Font) { textPane.setFont(font) }

  def windowSize = numLines
  def windowSize_=(numLines: Int) {
    this.numLines = numLines
  }
  def cursorPosition = _cursorPos
  def cursorPosition_=(pos: (Int, Int)) = {
    // TODO: check boundaries, if outside set to column 1 of current line
    //printf("@set_cursor -> (%d, %d)\n", pos._1, pos._2)
    _cursorPos = pos
  }
  def putChar(c: Char) {
    //printf("TopWindow.putChar at pos: (%d, %d): '%c'\n",
    //       _cursorPos._1, _cursorPos._2, c)
    if (c == '\n') moveCursorToNextLine
    else {
      val col  = _cursorPos._2
      val line = _cursorPos._1

      // new style
      buffer(line - 1, col - 1) = screenModel.styleCharacter(c)
      if (col < charsPerLine) _cursorPos = (line, col + 1)
    }
  }
  private def moveCursorToNextLine {
    val nextLine = math.min(numLines, _cursorPos._1 + 1)
    _cursorPos = (nextLine, 0)
  }

  def clear {
    //println("TOPWINDOW.CLEAR()")
    buffer.fillGridWith(TextStyles.DefaultFixedBlank, 0)
  }

  def reset {
    val currentSize: Dimension = textPane.getSize
    val fontMetrics = textPane.getGraphics.getFontMetrics(textPane.getFont)
    charsPerLine  = currentSize.width / fontMetrics.charWidth('0')
    totalLines    = currentSize.height / fontMetrics.getHeight
    buffer = new TextGridBuffer(totalLines, charsPerLine)
    //printf("SCREEN SIZE: %d LINES %d COLS\n", totalLines, charsPerLine)
    clear
  }
  def flush: Boolean = {
    val doc = textPane.getDocument
    doc.remove(0, doc.getLength)
    var row = 0
    var col = 0
    val attrs = textPane.getInputAttributes
    while (row < totalLines) {
      col = 0
      while (col < charsPerLine) {
        val styledChar = buffer(row, col)
        if (styledChar == TextStyles.DefaultFixedBlank) {
          screenModel.setTransparentAttributeSet(attrs)
        }
        else screenModel.setAttributeSet(attrs, styledChar)
        doc.insertString(
          doc.getLength,
          ((styledChar >>> 16) & 0xffff).asInstanceOf[Char].toString,
          attrs)
        col += 1
      }
      doc.insertString(doc.getLength, "\n", null)
      row += 1
    }
    true
  }
}

