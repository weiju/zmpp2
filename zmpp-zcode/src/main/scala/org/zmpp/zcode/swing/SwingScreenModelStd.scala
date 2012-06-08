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
import javax.swing.text.StyleConstants
import javax.swing.text.MutableAttributeSet
import java.awt.{FlowLayout,BorderLayout,GridLayout,Color,Font,Dimension,Graphics2D}
import java.awt.{Rectangle}
import java.awt.event._
import java.io.{FileOutputStream, FileInputStream}
import scala.collection.JavaConversions._
import org.zmpp.zcode._

/*
 * Implementation of the standard screen model using Swing components.
 */

/*
 * Standard screen model for all versions except 6.
 */
class SwingScreenModelStd(topWindow: TextGrid,
                          var DefaultBackground: Int = Colors.White,
                          var DefaultForeground: Int = Colors.Black)
extends JPanel(new BorderLayout)
with OutputStream with InputStream with SwingScreenModel with FocusListener {
  import ScrollPaneConstants._
  import ScreenModel._

  var vm: Machine       = null
  var activeWindowId    = BottomWindow // 0 is the bottom window, 1 is the top window
  var currentBackground = DefaultBackground
  var currentForeground = DefaultForeground
  var style             = TextStyles.Roman
  var currentFont       = Fonts.Normal
  val fixedFont         = new Font("Courier New", Font.PLAIN, 14)
  val stdFont           = new Font("American Typewriter", Font.PLAIN, 14)

  val statusBar    = new StatusBar
  val mainPane     = new JPanel(new BorderLayout)
  val bottomWindow = new TextBuffer(this)
  val scrollPane   = new JScrollPane(bottomWindow.textPane, VERTICAL_SCROLLBAR_NEVER,
                                     HORIZONTAL_SCROLLBAR_NEVER)

  // for timed input
  private[this] var interruptTask: InterruptTask = null

  ForegroundColors.setDefault(DefaultForeground)
  BackgroundColors.setDefault(DefaultBackground)

  scrollPane.setPreferredSize(new Dimension(640, 480))
  mainPane.add(scrollPane, BorderLayout.CENTER)

  add(mainPane, BorderLayout.CENTER)
  topWindow.addFocusListener(this)
  topWindow.screenModel = this

  def getComponent = this
  def activeWindow: ScreenModelWindow = activeWindowId match {
    case BottomWindow => bottomWindow
    case _ => topWindow
  }

  def connect(aVm: Machine) {
    vm = aVm
    remove(statusBar)
    if (vm.version <= 3) {
      add(statusBar, BorderLayout.NORTH)
    }
    if (vm.state.header.isBeyondZork) {
      // Beyond Zork only works well with White on Black !!!
      DefaultBackground = Colors.Black
      DefaultForeground = Colors.White
      ForegroundColors.setDefault(DefaultForeground)
      BackgroundColors.setDefault(DefaultBackground)
    }
  }

  def capabilities = List(SupportsColors,      SupportsBoldFont,   SupportsItalicFont,
                          SupportsFixedFont,   SupportsTimedInput, SupportsSound,
                          SupportsScreenSplit, SupportsMouse)

  import TextStyles._

  private[this] var selected      = true
  def isSelected            = selected
  def select(flag: Boolean) = selected = flag
  def putChar(c: Char) {
    if (SwingUtilities.isEventDispatchThread) _putChar(c)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _putChar(c)
      })
    }
  }
  def _putChar(c: Char) = activeWindow.putChar(vm.state.encoding.zsciiToUnicode(c))
  def _flush = {
    topWindow.flush
    bottomWindow.flush
  }
  def flush {
    if (SwingUtilities.isEventDispatchThread) _flush
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _flush
      })
    }
  }
  def flushInterruptOutput {
    SwingUtilities.invokeAndWait(new Runnable {
      def run = {
        _flush
        if (bottomWindow.isLineInputMode) {
          bottomWindow.setCaretToEnd(vm.readLineInfo.numLeftOverChars)
        }
      }
    })
  }

  def updateStatusLine {
    val objectName  = vm.statusLineObjectName
    val scoreOrTime = vm.statusLineScoreOrTime
    statusBar.set(objectName, scoreOrTime)
  }
  // input
  def readLine: Int = {
    flush
    val maxChars = vm.readLineInfo.maxInputChars
    //println("MAX_CHARS FOR READLINE: " + maxChars)
    if (vm.version <= 3) updateStatusLine
    scrollPane.getViewport.scrollRectToVisible(bottomRectangle)
    bottomWindow.requestLineInput(maxChars,
                                  vm.readLineInfo.numLeftOverChars)
    startInterruptTaskIfNeeded(vm.readLineInfo.routine,
                               vm.readLineInfo.time)
    0
  }
  private def bottomRectangle: Rectangle = {
    val right = bottomWindow.textPane.getWidth
    val bottom = bottomWindow.textPane.getHeight
    new Rectangle(0, bottom - 10, right, 10)
  }
  def readChar {
    if (vm.version <= 3) updateStatusLine
    flush
    bottomWindow.requestCharInput
    startInterruptTaskIfNeeded(vm.readCharInfo.routine, vm.readCharInfo.time)
  }
  private def startInterruptTaskIfNeeded(routine: Int, time: Int) {
    if (vm.version >= 4 && routine > 0 && time > 0) {
      interruptTask = new InterruptTask(vm, this, time, routine)
      interruptTask.start
    }
  }

  def cancelInput {
    // when this is called, the scheduler is guaranteed to not
    // execute any more interrupts
    if (interruptTask != null) {
      interruptTask = null
    }

    if (vm.state.runState == ZMachineRunStates.ReadChar) {
      vm.resumeWithCharInput(0)
    } else if (vm.state.runState == ZMachineRunStates.ReadLine) {
      vm.resumeWithLineInput("")
    }
    ExecutionControl.executeTurn(vm, this)
  }

  def resumeWithLineInput(input: String) {
    if (interruptTask != null) {
      interruptTask.shutdown
      interruptTask.await
      interruptTask = null
    }
    vm.resumeWithLineInput(input)
    ExecutionControl.executeTurn(vm, this)
  }
  def resumeWithCharInput(keyCode: Int) {
    //println("RESUME WITH " + keyCode)
    if (interruptTask != null) {
      interruptTask.shutdown
      interruptTask.await
      interruptTask = null
    }
    vm.resumeWithCharInput(keyCode)
    ExecutionControl.executeTurn(vm, this)
  }
  
  def screenOutputStream = this
  def keyboardStream     = this
  def screenModel        = this
  def splitWindow(lines: Int) {
    //println("@split_window, lines = " + lines)
    topWindow.windowSize = lines
    if (vm.version == 3) topWindow.clear
  }
  def setWindow(windowId: Int) {
    //println("@set_window, window id = " + windowId)
    if (windowId == BottomWindow || windowId == TopWindow) {
      activeWindowId = windowId
    } else {
      throw new IllegalArgumentException(
        "@set_window illegal window: %d".format(windowId))
    }
  }

  def cursorPosition: (Int, Int) = activeWindow.cursorPosition

  def setCursorPosition(line: Int, column: Int) {
    //printf("@set_cursor, line = %d, col = %d, active window: %d\n", line, column, activeWindowId)
    activeWindow.cursorPosition = (line, column)
  }

  def bufferMode(flag: Int) {
    //println("@buffer_mode, flag = " + flag)
    bottomWindow.useBufferMode = (flag != 0)
  }
  def eraseWindow(windowId: Int) {
    // TODO: polymorphism might make this prettier and shorter
    //println("@erase_window, win = " + windowId)
    if (windowId == -1) {
      topWindow.windowSize = 0
      topWindow.clear
      bottomWindow.clear
    } else if (windowId == -2) {
      topWindow.clear
      bottomWindow.clear
    } else if (windowId == TopWindow || windowId == 3 && activeWindowId == TopWindow) {
      topWindow.clear
    } else if (windowId == BottomWindow || windowId == 3 && activeWindowId == BottomWindow) {
      bottomWindow.clear
    }
  }
  def eraseLine(value: Int) {
    printf("@erase_line %d not implemented yet (TODO)\n", value)
  }
  def setTextStyle(aStyle: Int) {
    //printf("@set_style: %d\n", aStyle)
    bottomWindow.setStyle(aStyle)
    style = aStyle
  }

  // Note: window parameter is only relevant for V6
  // The method is called "setColour" only to express that it
  // implements the Z-instruction
  def setColour(foreground: Int, background: Int, window: Int) {
    //printf("setColour(), foreground = %d, background = %d\n",
    //       foreground, background)
    bottomWindow.setColor(foreground, background)

    // Make sure that we don't end up in an infinite loop
    // This could happen when we set currentForeground/currentBackground to CURRENT
    if (foreground != Colors.Current) currentForeground = foreground
    if (background != Colors.Current) currentBackground = background
    // we need to change the caret color of the bottom window, too
    //println("setting caret color")
    if (isReverseVideo(this.style)) {
      //println("reverse")
      bottomWindow.textPane.setCaretColor(getColor(background, false))
    } else {
      //println("normal video")
      val color = getColor(foreground, true)
      //println("color will be: " + color)
      bottomWindow.textPane.setCaretColor(color)
    }
    //println("exiting setColour")
  }

  private def getColor(colorId: Int, isForeground: Boolean): Color = {
    colorId match {
      case Colors.Current =>
        if (isForeground) getColor(currentForeground, true) else getColor(currentBackground, false)
      case _ =>
        if (isForeground) ForegroundColors(colorId) else BackgroundColors(colorId)
    }
  }
  def backgroundColor = getColor(currentBackground, false)
  def textColor       = getColor(currentForeground, true)

  def setFont(font: Int): Int = {
    if (isFontSupported(font)) {
      val previousFont = currentFont
      currentFont = font
      bottomWindow.setFont(font)
      previousFont
    } else 0
  }
  private def isFontSupported(font: Int): Boolean = {
    font == Fonts.Normal || font == Fonts.Fixed
  }

  def styleCharacter(c: Char) = {
    TextStyles.styleChar(c,
                         TextStyles.makeStyle(style,
                                              Fonts.Fixed,
                                              currentForeground,
                                              currentBackground))
  }
  val Transparent = new Color(0, 0, 0, 0)

  def setTransparentAttributeSet(attrs: MutableAttributeSet) {
    StyleConstants.setBackground(attrs, Transparent)
  }

  def setAttributeSet(attrs: MutableAttributeSet, styledChar: Int) {
    StyleConstants.setBold(attrs,
                           TextStyles.isBold(styledChar))
    StyleConstants.setItalic(attrs,
                             TextStyles.isItalic(styledChar))
    if (TextStyles.isReverseVideo(styledChar)) {
      StyleConstants.setBackground(
        attrs,
        getColor(TextStyles.foregroundColor(styledChar), true))
      StyleConstants.setForeground(
        attrs,
        getColor(TextStyles.backgroundColor(styledChar), false))
    } else {
      StyleConstants.setForeground(
        attrs,
        getColor(TextStyles.foregroundColor(styledChar), true))
      StyleConstants.setBackground(
        attrs,
        getColor(TextStyles.backgroundColor(styledChar), false))
    }
  }

  def attributeSetFor(attrs: MutableAttributeSet, style: Int) = {
    StyleConstants.setBold(attrs,   TextStyles.isBold(style))
    StyleConstants.setItalic(attrs, TextStyles.isItalic(style))
    if (TextStyles.isReverseVideo(style)) {
      //println("BOTTOM RUN - REVERSE VIDEO")
      StyleConstants.setBackground(attrs,
                                   getColor(TextStyles.foregroundColor(style),
                                            true))
      StyleConstants.setForeground(attrs,
                                   getColor(TextStyles.backgroundColor(style),
                                            false))
    } else {
/*
      printf("BOTTOM RUN - REGULAR VIDEO FG = %d, BG = %d\n",
             TextStyles.foregroundColor(style),
             TextStyles.backgroundColor(style))*/
      StyleConstants.setForeground(attrs,
                                   getColor(TextStyles.foregroundColor(style),
                                            true))
      StyleConstants.setBackground(attrs,
                                   getColor(TextStyles.backgroundColor(style),
                                            false))
    }
    if (currentFont == Fonts.Normal) {
      // TODO
    } else if (currentFont == Fonts.Fixed) {
      // TODO
    } else if (currentFont == Fonts.Picture) {
      throw new UnsupportedOperationException("Picture font not supported")
    } else if (currentFont == Fonts.CharacterGfx) {
      // TODO
      throw new UnsupportedOperationException("Character GFX font not supported")
    }
    attrs
  }

  def initUI {
    topWindow.setFont(fixedFont)
    bottomWindow.textPane.setFont(stdFont)
    topWindow.reset
    bottomWindow.reset

    // now the top window "knows" how large the screen is, so we can set
    // the dimensions and font sizes to the VM
    vm.setFontSizeInUnits(1, 1)
    vm.setScreenSizeInUnits(topWindow.charsPerLine, topWindow.totalLines)
  }

  // For now, this is just an approximation, we just prevent that
  // the cursor is caught in the glass pane while we actually want to have
  // input focus in the bottom window in the normal case.
  // E.g. "Deadline" will not look as good with this approach, a better one
  // would take into account how large the top window is and allow focus
  // in case the cursor is set in the visible area of the top window
  def focusGained(e: FocusEvent) = bottomWindow.textPane.requestFocusInWindow
  def focusLost(e: FocusEvent) { }

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
}
