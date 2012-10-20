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

import javax.swing.JTextPane
import java.awt.Graphics2D
import java.awt.event.{KeyListener, KeyEvent}
import scala.collection.JavaConversions._
import org.zmpp.zcode.{ScreenModelWindow, TextRunBuffer}


object TextBuffer {
  val MarginLeft   = 20
  val MarginRight  = 20
  val MarginTop    = 10
  val MarginBottom = 10
}

class TextBuffer(screenModel: SwingScreenModel)
extends ScreenModelWindow with KeyListener {
  val textPane = new JTextPane
  textPane.setMargin(new java.awt.Insets(TextBuffer.MarginTop,
                                TextBuffer.MarginLeft,
                                TextBuffer.MarginBottom,
                                TextBuffer.MarginRight))
  textPane.addKeyListener(this)
  val runBuffer = new TextRunBuffer

  var inputMode = TextInputMode.InputNone
  var useBufferMode = true
  def isCharInputMode = inputMode == TextInputMode.ReadChar
  def isLineInputMode = inputMode == TextInputMode.ReadLine
  private var inputStart    = 0
  private var maxInputChars = 0

  def reset {
    inputMode     = TextInputMode.InputNone
    runBuffer.reset
    inputStart    = 0
    maxInputChars = 0
    clear
  }

  def setStyle(style: Int) = runBuffer.setStyle(style)
  def setFont(fontnum: Int) = runBuffer.setFont(fontnum)
  def setColor(foreground: Int, background: Int) = runBuffer.setColor(foreground, background)

  private def attributeSetFor(style: Int) = {
    val attrs = textPane.getInputAttributes
    screenModel.attributeSetFor(attrs, style)
    attrs
  }

  private def numRows = {
    val g2d = textPane.getGraphics.asInstanceOf[Graphics2D]
    val lineMetrics = screenModel.stdFont.getLineMetrics("0", g2d.getFontRenderContext)
    (textPane.getHeight / lineMetrics.getHeight).toInt + 1
  }

  def clear {
    val clearScreenBuilder = new StringBuilder()
    // preserve the current style when doing a clear !
    runBuffer.clear
    //println("Bottom Window has " + numRows + " rows.")
    (1 to numRows).foreach(_ => clearScreenBuilder.append('\n'))
    textPane.setText(clearScreenBuilder.toString)
    textPane.setBackground(screenModel.backgroundColor)
    setCaretToEnd(0)
  }

  def setCaretToEnd(numLeftOverChars: Int) {
    inputStart    = textPane.getDocument.getLength
    textPane.setCaretPosition(inputStart)
    inputStart -= numLeftOverChars
  }

  def putChar(c: Char) {
    runBuffer.append(c)
  }
  def flush: Boolean = {
    val doc = textPane.getDocument
    val styledRuns = runBuffer.grabRuns
    for (styledRun <- styledRuns) {
      doc.insertString(doc.getLength, styledRun.text, attributeSetFor(styledRun.style))
    }
    true
  }
  
  // ****** KeyListener ******
  def keyPressed(event: KeyEvent) {
    import KeyEvent._
    if (isCharInputMode) {
      val keyChar = event.getKeyChar
      if (keyChar == VK_ENTER) {
        screenModel.resumeWithCharInput(13)
      } else if (keyChar != CHAR_UNDEFINED) {
        screenModel.resumeWithCharInput(keyChar)
      } else {
        val keyCode = event.getKeyCode
        keyCode match {
          case VK_ENTER   => screenModel.resumeWithCharInput(13)
          case VK_UP      => screenModel.resumeWithCharInput(129)
          case VK_DOWN    => screenModel.resumeWithCharInput(130)
          case VK_LEFT    => screenModel.resumeWithCharInput(131)
          case VK_RIGHT   => screenModel.resumeWithCharInput(132)
          case VK_F1      => screenModel.resumeWithCharInput(133)
          case VK_F2      => screenModel.resumeWithCharInput(134)
          case VK_F3      => screenModel.resumeWithCharInput(135)
          case VK_F4      => screenModel.resumeWithCharInput(136)
          case VK_F5      => screenModel.resumeWithCharInput(137)
          case VK_F6      => screenModel.resumeWithCharInput(138)
          case VK_F7      => screenModel.resumeWithCharInput(139)
          case VK_F8      => screenModel.resumeWithCharInput(140)
          case VK_F9      => screenModel.resumeWithCharInput(141)
          case VK_F10     => screenModel.resumeWithCharInput(142)
          case VK_F11     => screenModel.resumeWithCharInput(143)
          case VK_F12     => screenModel.resumeWithCharInput(144)
          case VK_NUMPAD0 => screenModel.resumeWithCharInput(145)
          case VK_NUMPAD1 => screenModel.resumeWithCharInput(146)
          case VK_NUMPAD2 => screenModel.resumeWithCharInput(147)
          case VK_NUMPAD3 => screenModel.resumeWithCharInput(148)
          case VK_NUMPAD4 => screenModel.resumeWithCharInput(149)
          case VK_NUMPAD5 => screenModel.resumeWithCharInput(150)
          case VK_NUMPAD6 => screenModel.resumeWithCharInput(151)
          case VK_NUMPAD7 => screenModel.resumeWithCharInput(152)
          case VK_NUMPAD8 => screenModel.resumeWithCharInput(153)
          case VK_NUMPAD9 => screenModel.resumeWithCharInput(154)
        }
      }
    } else if (isLineInputMode) {
      val doc = textPane.getDocument
      val caretPos = textPane.getCaret.getDot
      if (caretPos < inputStart) textPane.getCaret.setDot(doc.getLength)

      if (event.getKeyCode == KeyEvent.VK_ENTER) {
        event.consume
        val input = doc.getText(inputStart, doc.getLength - inputStart)
        doc.insertString(doc.getLength, "\n", null)
        //println("Input was: " + input)
        screenModel.resumeWithLineInput(input + "\n")          
        inputMode = TextInputMode.InputNone
      } else if (event.getKeyCode == KeyEvent.VK_BACK_SPACE ||
                 event.getKeyCode == KeyEvent.VK_LEFT) {
        if (textPane.getCaret.getDot <= inputStart) event.consume
      } else if (event.getKeyCode == KeyEvent.VK_UP) {
        event.consume
      } else if (doc.getLength - inputStart >= maxInputChars) {
        // eat the non-visible characters that go over the text buffer
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
    } else if (!isLineInputMode) {
      // not in input mode, eat all key events
      event.consume
    } else if (textPane.getDocument.getLength - inputStart >= maxInputChars) {
      // we need to consume twice in order to eat the visible characters
      // in line input mode
      event.consume
    }
  }
  def keyReleased(event: KeyEvent) {}

  // input
  // has to be called in UI event thread
  def requestLineInput(maxChars: Int, numLeftOverChars: Int) {
    //println("requestLineInput")
    textPane.requestFocusInWindow
    textPane.getCaret.setVisible(true)
    setCaretToEnd(numLeftOverChars)
    maxInputChars = maxChars
    inputMode     = TextInputMode.ReadLine
  }
  def requestCharInput {
    textPane.requestFocusInWindow
    textPane.getCaret.setVisible(true)
    inputMode = TextInputMode.ReadChar
  }

  def cursorPosition = {
    throw new UnsupportedOperationException("@get_cursor not supported for bottom window")
  }
  def cursorPosition_=(pos: (Int, Int)) = {
    println("warn: @set_cursor not supported for bottom window")
  }
}
