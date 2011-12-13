/*
 * Created on 2010/08/07
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
package org.zmpp.zcode

import javax.swing._
import javax.swing.text.StyleConstants
import javax.swing.text.MutableAttributeSet
import java.awt.{FlowLayout,BorderLayout,GridLayout,Color,Font,Dimension,Graphics2D}
import java.awt.{Rectangle}
import java.awt.event._
import java.io.{FileOutputStream, FileInputStream}

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
    _cursorPos = pos
  }
  def putChar(c: Char) {
    if (c == '\n') moveCursorToNextLine
    else {
      val col  = _cursorPos._2
      val line = _cursorPos._1

      // new style
      buffer.putChar(styleCharacter(c), line - 1, col -1)
      if (col < charsPerLine) _cursorPos = (line, col + 1)
    }
  }
  private def moveCursorToNextLine {
    val nextLine = math.min(numLines, _cursorPos._1 + 1)
    _cursorPos = (nextLine, 0)
  }

  private def styleCharacter(c: Char): StyledChar = {
    screenModel.styleCharacter(c)
  }

  def clear {
    println("TOPWINDOW.CLEAR()")
    buffer.fillGridWith(TextStyles.DefaultFixedBlank, 0)
  }

  def reset {
    val currentSize: Dimension = getSize
    val fontMetrics = getGraphics.getFontMetrics(getFont)
    charsPerLine  = currentSize.width / fontMetrics.charWidth('0')
    totalLines    = currentSize.height / fontMetrics.getHeight
    buffer = new TextGridBuffer(totalLines, charsPerLine)
    printf("SCREEN SIZE: %d LINES %d COLS\n", totalLines, charsPerLine)
    clear
  }

  def flush {
    val doc = getDocument
    doc.remove(0, doc.getLength)
    var row = 0
    var col = 0
    val attrs = getInputAttributes
    while (row < totalLines) {
      col = 0
      while (col < charsPerLine) {
        val styledChar = buffer.charAt(row, col)
        if (styledChar == TextStyles.DefaultFixedBlank) {
          screenModel.setTransparentAttributeSet(attrs)
        }
        else screenModel.setAttributeSet(attrs, styledChar)
        doc.insertString(doc.getLength, styledChar.c.toString, attrs)
        col += 1
      }
      doc.insertString(doc.getLength, "\n", null)
      row += 1
    }
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

  private def attributeSetFor(style: TextStyle) = {
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
    // TODO: we might want to incorporate the run buffer here
    // TODO: also take into account the background color
    val clearScreenBuilder = new StringBuilder()
    println("Bottom Window has " + numRows + " rows.")
    (1 to numRows).foreach(_ => clearScreenBuilder.append('\n'))
    setText(clearScreenBuilder.toString)
    setBackground(screenModel.backgroundColor)
    setCaretToEnd
  }

  private def setCaretToEnd {
    inputStart    = getDocument.getLength
    setCaretPosition(inputStart)
  }

  def putChar(c: Char) {
    runBuffer.append(c)
  }
  def flush {
    val doc = getDocument
    val styledRuns = runBuffer.grabRuns
    for (styledRun <- styledRuns) {
      doc.insertString(doc.getLength, styledRun.text, attributeSetFor(styledRun.style))
    }
  }
  
  // ****** KeyListener ******
  def keyPressed(event: KeyEvent) {
    if (isCharInputMode) {
      screenModel.resumeWithCharInput(event.getKeyChar)
    } else if (isLineInputMode) {
      val doc = getDocument
      val caretPos = getCaret.getDot
      if (caretPos < inputStart) getCaret.setDot(doc.getLength)

      if (event.getKeyCode == KeyEvent.VK_ENTER) {
        event.consume
        val input = doc.getText(inputStart, doc.getLength - inputStart)
        doc.insertString(doc.getLength, "\n", null)
        println("Input was: " + input)
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
  def requestLineInput(maxChars: Int) {
    println("requestLineInput")
    requestFocusInWindow
    getCaret.setVisible(true)
    setCaretToEnd
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
  }

  def capabilities = List(SupportsColors,      SupportsBoldFont,   SupportsItalicFont,
                          SupportsFixedFont,   SupportsTimedInput, SupportsSound,
                          SupportsScreenSplit, SupportsMouse)

  import TextStyles._
/*
  def isRoman        = style                  == Roman
  def isReverseVideo = (style & ReverseVideo) == ReverseVideo
  def isBold         = (style & Bold)         == Bold
  def isItalic       = (style & Italic)       == Italic
  def isFixedStyle   = (style & FixedPitch)   == FixedPitch
*/
  private var selected      = true
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

  def updateStatusLine {
    val objectName  = vm.statusLineObjectName
    val scoreOrTime = vm.statusLineScoreOrTime
    statusBar.set(objectName, scoreOrTime)
  }
  // input
  def readLine: Int = {
    val maxChars = vm.readLineInfo.maxInputChars
    println("MAX_CHARS FOR READLINE: " + maxChars)
    if (vm.version <= 3) updateStatusLine
    scrollPane.getViewport.scrollRectToVisible(bottomRectangle)
    bottomWindow.requestLineInput(maxChars)
    0
  }
  private def bottomRectangle: Rectangle = {
    val right = bottomWindow.getWidth
    val bottom = bottomWindow.getHeight
    new Rectangle(0, bottom - 10, right, 10)
  }
  def readChar {
    if (vm.version <= 3) updateStatusLine
    bottomWindow.requestCharInput
  }

  def resumeWithLineInput(input: String) {
    println("RESUME WITH " + input)
    vm.resumeWithLineInput(input)
    ExecutionControl.executeTurn(vm, this)
  }
  def resumeWithCharInput(keyCode: Int) {
    println("RESUME WITH " + keyCode)
    vm.resumeWithCharInput(keyCode)
    ExecutionControl.executeTurn(vm, this)
  }
  
  def screenOutputStream = this
  def keyboardStream     = this
  def screenModel        = this
  def splitWindow(lines: Int) {
    println("@split_window, lines = " + lines)
    topWindow.windowSize = lines
    if (vm.version == 3) topWindow.clear
  }
  def setWindow(windowId: Int) {
    println("@set_window, window id = " + windowId)
    if (windowId == BottomWindow || windowId == TopWindow) {
      activeWindowId = windowId
    } else {
      throw new IllegalArgumentException(
        "@set_window illegal window: %d".format(windowId))
    }
  }

  def cursorPosition: (Int, Int) = activeWindow.cursorPosition

  def setCursorPosition(line: Int, column: Int) {
    println("@set_cursor, line = " + line + " col = " + column)
    activeWindow.cursorPosition = (line, column)
  }

  def bufferMode(flag: Int) {
    println("@buffer_mode, flag = " + flag)
    bottomWindow.useBufferMode = (flag != 0)
  }
  def eraseWindow(windowId: Int) {
    // TODO: polymorphism might make this prettier and shorter
    println("@erase_window, win = " + windowId)
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
    bottomWindow.setStyle(aStyle)
    style = aStyle
  }

  // Note: window parameter is only relevant for V6
  // The method is called "setColour" only to express that it
  // implements the Z-instruction
  def setColour(foreground: Int, background: Int, window: Int) {
    printf("setColour(), foreground = %d, background = %d, window = %d\n",
           foreground, background, window)
    bottomWindow.setColor(foreground, background)

    // Make sure that we don't end up in an infinite loop
    // This could happen when we set currentForeground/currentBackground to CURRENT
    if (foreground != Colors.Current) currentForeground = foreground
    if (background != Colors.Current) currentBackground = background
    // we need to change the caret color of the bottom window, too
    println("setting caret color")
    if (isReverseVideo(this.style)) {
      println("reverse")
      bottomWindow.setCaretColor(getColor(background, false))
    } else {
      println("normal video")
      val color = getColor(foreground, true)
      println("color will be: " + color)
      bottomWindow.setCaretColor(color)
    }
    println("exiting setColour")
  }

  private def getColor(colorId: Int, isForeground: Boolean): Color = {
    colorId match {
      case Colors.Current =>
        if (isForeground) getColor(currentForeground, true) else getColor(currentBackground, true)
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
    StyledChar(c, TextStyle(isItalic(style), isBold(style), isReverseVideo(style),
               Fonts.Fixed, currentForeground, currentBackground))
  }
  val Transparent = new Color(0, 0, 0, 0)

  def setTransparentAttributeSet(attrs: MutableAttributeSet) = {
    StyleConstants.setBackground(attrs, Transparent)
  }

  def setAttributeSet(attrs: MutableAttributeSet, styledChar: StyledChar) = {
    StyleConstants.setBold(attrs,   styledChar.isBold)
    StyleConstants.setItalic(attrs, styledChar.isItalic)
    if (styledChar.isReverseVideo) {
      StyleConstants.setBackground(attrs, getColor(styledChar.foreground, true))
      StyleConstants.setForeground(attrs, getColor(styledChar.background, false))
    } else {
      StyleConstants.setForeground(attrs, getColor(styledChar.foreground, true))
      StyleConstants.setBackground(attrs, getColor(styledChar.background, false))
    }
  }

  def attributeSetFor(attrs: MutableAttributeSet, style: TextStyle) = {
    StyleConstants.setBold(attrs,   style.isBold)
    StyleConstants.setItalic(attrs, style.isItalic)
    if (style.isReverseVideo) {
      StyleConstants.setBackground(attrs, getColor(style.foreground, true))
      StyleConstants.setForeground(attrs, getColor(style.background, false))
    } else {
      StyleConstants.setForeground(attrs, getColor(style.foreground, true))
      StyleConstants.setBackground(attrs, getColor(style.background, false))
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
    println("INITIALIZE THE UI !!!")
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
