/*
 * Created on 2010/08/07
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
package org.zmpp.zcode

import javax.swing._
import javax.swing.text.StyleConstants
import javax.swing.text.MutableAttributeSet
import java.awt.{FlowLayout,BorderLayout,GridLayout,Color,Font,Dimension,Graphics2D}
import java.awt.{Rectangle}
import java.awt.event._
import java.io.{FileOutputStream, FileInputStream}
import scala.collection.JavaConversions._

import java.util.concurrent.{Executors, TimeUnit}
/*
 * Implementation of the standard screen model using Swing components.
 */

// The V1-V3 status bar. This is implemented as a separate component
// because there is at least 1 game (Seastalker) which has both status
// bar and two windows
class StatusBar extends JPanel(new GridLayout(1, 2)) {
  private val objectLabel = new JLabel(" ")
  private val scoreLabel       = new JLabel(" ")
  private val left  = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private val right = new JPanel(new FlowLayout(FlowLayout.RIGHT))
  this.add(left)
  this.add(right)
  left.add(objectLabel)
  right.add(scoreLabel)
  def set(objectName: String, scoreOrTime: String) {
    objectLabel.setText(objectName)
    scoreLabel.setText(scoreOrTime)
  }
}

// Define _two_ color tables, one for background colors
// and one for foreground tables. The reason is that some
// games (e.g. Varicella) don't really deal with colors properly.
// They rely on the foreground color being brighter than the foreground
// color. Unfortunately, Varicella also assumes that the default foreground
// color is not black.
object BackgroundColors {
  val colorTable = new Array[Color](13)
  // current is never accessed and default is set by screen model
  colorTable(Colors.Black)   = Color.BLACK
  colorTable(Colors.Red)     = new Color(200, 0, 0)
  colorTable(Colors.Green)   = new Color(0, 200, 0)
  colorTable(Colors.Yellow)  = new Color(200, 200, 0)
  colorTable(Colors.Blue)    = new Color(0, 0, 200)
  colorTable(Colors.Magenta) = new Color(200, 0, 200)
  colorTable(Colors.Cyan)    = new Color(0, 200, 200)
  colorTable(Colors.White)   = new Color(255, 255, 255)

  def apply(colornum: Int): Color = {
    colorTable(colornum)
  }
  def setDefault(colornum: Int) {
    colorTable(Colors.Default) = colorTable(colornum)
  }
}
object ForegroundColors {
  val Black = new Color(60, 60, 60)
  val colorTable = new Array[Color](13)
  // current is never accessed and default is set by screen model
  colorTable(Colors.Black)   = Black
  colorTable(Colors.Red)     = new Color(255, 0, 0)
  colorTable(Colors.Green)   = new Color(0, 255, 0)
  colorTable(Colors.Yellow)  = new Color(255, 255, 0)
  colorTable(Colors.Blue)    = new Color(0, 0, 255)
  colorTable(Colors.Magenta) = new Color(255, 0, 255)
  colorTable(Colors.Cyan)    = new Color(0, 255, 255)
  colorTable(Colors.White)   = new Color(200, 200, 200)

  def apply(colornum: Int): Color = {
    colorTable(colornum)
  }
  def setDefault(colornum: Int) {
    colorTable(Colors.Default) = colorTable(colornum)
  }
}

// A class to implement the top window. In Swing, the top window sits
// in the glass pane. This is done to implement the tricky behaviour
// of the Z-machine screen model of overlaying bottom window in a
// possibly overlapping way.
// The standard layout containers either don't overlap or do not
// show the stacked components below. This is also one of the reasons
// why the Glk can't fully implement the Z-machine screen model, V6
// is a another story.
class TextGrid extends JTextPane with ScreenModelWindow {

  private var numLines     = 0
  private var _cursorPos   = (1, 1)
  // Even though the text grid is a non-buffered window type, we still buffer
  // it up to prepare for resizing of the main window
  var buffer: TextGridBuffer = null
  var totalLines   = 0
  var charsPerLine = 0

  // we need access to the screen model to access
  var screenModel    : SwingScreenModelStd = null

  setOpaque(false)

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
    val currentSize: Dimension = getSize
    val fontMetrics = getGraphics.getFontMetrics(getFont)
    charsPerLine  = currentSize.width / fontMetrics.charWidth('0')
    totalLines    = currentSize.height / fontMetrics.getHeight
    buffer = new TextGridBuffer(totalLines, charsPerLine)
    //printf("SCREEN SIZE: %d LINES %d COLS\n", totalLines, charsPerLine)
    clear
  }
  def flush: Boolean = {
    val doc = getDocument
    doc.remove(0, doc.getLength)
    var row = 0
    var col = 0
    val attrs = getInputAttributes
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

object TextBuffer {
  val MarginLeft   = 20
  val MarginRight  = 20
  val MarginTop    = 10
  val MarginBottom = 10
}

object TextInputMode extends Enumeration {
  val InputNone = Value("None")
  val ReadLine  = Value("ReadLine")
  val ReadChar  = Value("ReadChar")
} 

class TextBuffer(screenModel: SwingScreenModelStd)
extends JTextPane with ScreenModelWindow with KeyListener {
  setMargin(new java.awt.Insets(TextBuffer.MarginTop,
                                TextBuffer.MarginLeft,
                                TextBuffer.MarginBottom,
                                TextBuffer.MarginRight))
  addKeyListener(this)
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
    val attrs = getInputAttributes
    screenModel.attributeSetFor(attrs, style)
    attrs
  }

  private def numRows = {
    val g2d = getGraphics.asInstanceOf[Graphics2D]
    val lineMetrics = screenModel.stdFont.getLineMetrics("0", g2d.getFontRenderContext)
    (getHeight / lineMetrics.getHeight).toInt + 1
  }

  def clear {
    val clearScreenBuilder = new StringBuilder()
    // preserve the current style when doing a clear !
    runBuffer.clear
    //println("Bottom Window has " + numRows + " rows.")
    (1 to numRows).foreach(_ => clearScreenBuilder.append('\n'))
    setText(clearScreenBuilder.toString)
    setBackground(screenModel.backgroundColor)
    setCaretToEnd(0)
  }

  def setCaretToEnd(numLeftOverChars: Int) {
    inputStart    = getDocument.getLength
    setCaretPosition(inputStart)
    inputStart -= numLeftOverChars
  }

  def putChar(c: Char) {
    runBuffer.append(c)
  }
  def flush: Boolean = {
    val doc = getDocument
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
      val doc = getDocument
      val caretPos = getCaret.getDot
      if (caretPos < inputStart) getCaret.setDot(doc.getLength)

      if (event.getKeyCode == KeyEvent.VK_ENTER) {
        event.consume
        val input = doc.getText(inputStart, doc.getLength - inputStart)
        doc.insertString(doc.getLength, "\n", null)
        //println("Input was: " + input)
        screenModel.resumeWithLineInput(input + "\n")          
        inputMode = TextInputMode.InputNone
      } else if (event.getKeyCode == KeyEvent.VK_BACK_SPACE ||
                 event.getKeyCode == KeyEvent.VK_LEFT) {
        if (getCaret.getDot <= inputStart) event.consume
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
    } else if (getDocument.getLength - inputStart >= maxInputChars) {
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
    requestFocusInWindow
    getCaret.setVisible(true)
    setCaretToEnd(numLeftOverChars)
    maxInputChars = maxChars
    inputMode     = TextInputMode.ReadLine
  }
  def requestCharInput {
    requestFocusInWindow
    getCaret.setVisible(true)
    inputMode = TextInputMode.ReadChar
  }

  def cursorPosition = {
    throw new UnsupportedOperationException("@get_cursor not supported for bottom window")
  }
  def cursorPosition_=(pos: (Int, Int)) = {
    throw new UnsupportedOperationException("@set_cursor not supported for bottom window")
  }
}

class InterruptTask(vm: Machine, screenModel: ScreenModel,
                    time: Int, routine: Int) {
  private[this] val scheduler = Executors.newScheduledThreadPool(1)

  val runner = new Runnable {
    def run {
      val oldfp = vm.state.fp
      vm.callInterrupt(routine)
      while (vm.state.fp != oldfp) {
        vm.doInstruction(false)
      }
      screenModel.flushInterruptOutput

      if (vm.state.thrownAwayValue == 1) {
        scheduler.shutdown
        screenModel.cancelInput
      }
    }
  }
  def shutdown = scheduler.shutdown

  def start {
    val future = scheduler.scheduleAtFixedRate(runner, 0,
                                               time * 100,
                                               TimeUnit.MILLISECONDS)
  }
  def await {
    scheduler.awaitTermination(50, TimeUnit.SECONDS)
  }
}

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
  val scrollPane   = new JScrollPane(bottomWindow, VERTICAL_SCROLLBAR_NEVER,
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
    val right = bottomWindow.getWidth
    val bottom = bottomWindow.getHeight
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
      bottomWindow.setCaretColor(getColor(background, false))
    } else {
      //println("normal video")
      val color = getColor(foreground, true)
      //println("color will be: " + color)
      bottomWindow.setCaretColor(color)
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

  def setTransparentAttributeSet(attrs: MutableAttributeSet) = {
    StyleConstants.setBackground(attrs, Transparent)
  }

  def setAttributeSet(attrs: MutableAttributeSet, styledChar: Int) = {
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
    bottomWindow.setFont(stdFont)
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
  def focusGained(e: FocusEvent) = bottomWindow.requestFocusInWindow
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
