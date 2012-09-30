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
import javax.swing.text.MutableAttributeSet
import java.awt.{Dimension,Font, Color}
import java.io.{FileInputStream, FileOutputStream}
import org.zmpp.zcode._
import org.zmpp.zcode.ScreenModel._

abstract class V6WindowState(var left: Int, var top: Int, var width: Int, var height: Int) {
  def setCursor(line: Int, column: Int) {
    throw new UnsupportedOperationException
  }
  def eraseLine(value: Int) {
    throw new UnsupportedOperationException
  }
}

// units are pixels
class V6GraphicsWindow(width: Int, height: Int) extends V6WindowState(0, 0, width, height)
class V6NullWindowState(width: Int, height: Int) extends V6WindowState(0, 0, width, height)
class V6TextGrid(width: Int, height: Int) extends V6WindowState(0, 0, width, height) {
  override def setCursor(line: Int, column: Int) {
    printf("@set_cursor l = %d r = %d (TextGrid - TODO)\n", line, column)
  }
  override def eraseLine(value: Int) {
    printf("@erase_line v = %d (TextGrid - TODO)\n", value)
  }
}
class V6TextWindow(width: Int, height: Int) extends V6WindowState(0, 0, width, height) {
  override def setCursor(line: Int, column: Int) {
    printf("@set_cursor l = %d r = %d (TextWindow - TODO)\n", line, column)
  }
  override def eraseLine(value: Int) {
    printf("@erase_line v = %d (TextWindow - TODO)\n", value)
  }
}

class V6Window {
  private[this] var _currentState: V6WindowState = new V6NullWindowState(0, 0)
  def currentState = _currentState
  def currentState_=(state: V6WindowState) {
    _currentState = state
  }

  def clear { }
  def reset {
    _currentState.left = 0
    _currentState.top = 0
    setBounds(0, 0, 0, 0)
  }
  def setBounds(left: Int, top: Int, xunits: Int, yunits: Int) {
    _currentState.left = left
    _currentState.top = top
    _currentState.width = xunits
    _currentState.height = yunits
  }
  def setCursor(line: Int, column: Int) {
    _currentState.setCursor(line, column)
  }
  def eraseLine(value: Int) {
    _currentState.eraseLine(value)
  }
}

/*
 * Implementation of the V6 screen model using Swing components.
 * The V6 screen model is pixel-based, so rendering is entirely done
 * in a custom way, making this much more difficult and more restrictive.
 */
class SwingScreenModelV6 extends JComponent
with OutputStream with InputStream with SwingScreenModel {
  private[this] var vm: Machine = null
  private[this] val windows = Array.ofDim[V6Window](8)
  private[this] var selected  = true
  private[this] val fixedFont         = new Font("Courier New", Font.PLAIN, 14)
  private[this] var selectedWindowId = BottomWindow
  private[this] var currentStyle = TextStyles.Roman

  (0 until 8).foreach{ i => windows(i) = new V6Window }
  setPreferredSize(new Dimension(640, 480))

  def getComponent = this
  def capabilities = List(SupportsColors,    SupportsBoldFont,    SupportsItalicFont,
                          SupportsFixedFont, SupportsTimedInput,  SupportsSound,
                          SupportsPictures,  SupportsScreenSplit, SupportsMouse,
                          SupportsMenus)
  def activeWindow = {
    throw new UnsupportedOperationException("Not supported yet")
  }

  // OutputStream
  def isSelected = selected
  def select(flag: Boolean) = selected = flag
  def putChar(c: Char) {
    //println("@put_char: '%c'".format(c))
  }
  def flush { }
  def flushInterruptOutput { }
  def cancelInput { }

  // InputStream
  def readLine: Int = {
    println("@read_line (TODO)")
    0
  }

  // ScreenModel
  private def resetScreen {
    val g = getGraphics
    g.clearRect(0, 0, getWidth, getHeight)
    (0 until 8).foreach{ i => windows(i).reset }
    windows(BottomWindow).setBounds(0, 0, getWidth, getHeight)
  }

  def initUI {
    // now the top window "knows" how large the screen is, so we can set
    // the dimensions and font sizes to the VM
    val g = getGraphics
    val fm = g.getFontMetrics(fixedFont)
    vm.setFontSizeInUnits(fm.charWidth('0'), fm.getHeight)
    vm.setScreenSizeInUnits(getWidth, getHeight)
    
    println("Screen size (units): " + vm.screenSizeInUnits)
    println("Font size (units): " + vm.fontSizeInUnits)
    windows(BottomWindow).currentState = new V6TextWindow(getWidth, getHeight)
    windows(TopWindow).currentState = new V6TextGrid(0, 0)
  }
  def connect(aVm: Machine) {
    vm = aVm
  }
  def setColour(foreground: Int, background: Int, window: Int) {
    printf("@set_colour %d %d %d (TODO)\n", foreground, background, window)
  }
  def setFont(font: Int): Int = {
    printf("@set_font %d (TODO)\n", font)
    1
  }
  def eraseLine(value: Int) {
    windows(selectedWindowId).eraseLine(value)
  }
  def setTextStyle(aStyle: Int) {
    // TODO: style is actually a screen-global property
    currentStyle = aStyle
    printf("@set_text_style %d\n", aStyle)
  }
  def eraseWindow(windowId: Int) {
    windowId match {
      case -1 => resetScreen
      case -2 =>
        println("clear window, no unsplit (TODO)")
      case _ =>
        println("clear selected window (TODO)")
  }
  }
  def setWindow(windowId: Int) {
    printf("@set_window %d\n", windowId)
    selectedWindowId = windowId
  }
  def splitWindow(lines: Int) {
    printf("@split_window (%d units) [V6 emulation] (TODO)\n", lines)
    val fontSize = vm.fontSizeInUnits
    windows(BottomWindow).setBounds(0, fontSize._2, getWidth, getHeight - fontSize._2)
    windows(TopWindow).setBounds(0, 0, getWidth, fontSize._2)
  }
  def cursorPosition: (Int, Int) = {
    throw new UnsupportedOperationException("getCursorPosition() not yet implemented in screen model")
  }
  def setCursorPosition(line: Int, column: Int) {
    windows(selectedWindowId).setCursor(line, column)
  }
  def updateStatusLine { }
  def screenOutputStream = this
  def keyboardStream     = this
  def bufferMode(flag: Int) {
    printf("@buffer_mode %d (TODO)\n", flag)
  }

  // SwingStreamModel
  def readChar { }

  def requestSaveFile {
    val fileChooser = new JFileChooser
    fileChooser.setDialogTitle("Save Game As...")
    val outputStream = if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
      new FileOutputStream(fileChooser.getSelectedFile)
    } else null
    vm.resumeWithSaveStream(outputStream)
    ExecutionControl.executeTurn(vm, this)
  }
  def requestRestoreFile {
    val fileChooser = new JFileChooser
    fileChooser.setDialogTitle("Restore Game From...")
    val inputStream = if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
      new FileInputStream(fileChooser.getSelectedFile)
    } else null
    vm.resumeWithRestoreStream(inputStream)
    ExecutionControl.executeTurn(vm, this)
  }

  // empty for now
  def styleCharacter(c: Char): Int = 0
  def setTransparentAttributeSet(attrs: MutableAttributeSet) {}
  def setAttributeSet(attrs: MutableAttributeSet, styledChar: Int) {}
  def resumeWithCharInput(c: Int) {}
  def resumeWithLineInput(line: String) {}
  def attributeSetFor(attrs: MutableAttributeSet, style: Int): MutableAttributeSet = null
  def stdFont: Font = null
  def backgroundColor: Color = null
}
