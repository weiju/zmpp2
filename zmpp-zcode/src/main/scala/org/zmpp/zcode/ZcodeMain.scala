/*
 * Created on 2010/05/12
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
package org.zmpp.zcode

import java.util.logging._
import java.io.File
import java.io.FileInputStream

import org.zmpp.base.Memory
import org.zmpp.base.DefaultMemory
import org.zmpp.base.VMRunStates

import javax.swing._
import java.awt._
import java.awt.event._

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

// A class to implement the top window. In Swing, the top window sits
// in the glass pane. This is done to implement the tricky behaviour
// of the Z-machine screen model of overlaying bottom window in a
// possibly overlapping way.
// The standard layout containers either don't overlap or do not
// show the stacked components below. This is also one of the reasons
// why the Glk can't fully implement the Z-machine screen model, V6
// is a another story.
class TextGrid extends JTextPane {
  private var numLines     = 0
  private var _cursorPos   = (1, 1)
  // TODO: Use annotated characters
  private var buffer : Array[Array[Char]] = null
  var totalLines   = 0
  var charsPerLine = 0

  setOpaque(false)

  def windowSize = numLines
  def windowSize_=(numLines: Int) {
    this.numLines = numLines
  }
  def cursorPos = _cursorPos
  def cursorPos_=(pos: (Int, Int)) = {
    // TODO: check boundaries, if outside set to column 1 of current line
    _cursorPos = pos
  }
  def putChar(c: Char) {
    if (c == '\n') moveCursorToNextLine
    else {
      val col  = _cursorPos._2
      val line = _cursorPos._1
      //printf("PUT CHARACTER TO POS (%d, %d)\n", line, col)
      // save the state in the buffer
      buffer(line - 1)(col - 1) = c
      // and also set it in the window
      val offset = (line - 1) * (charsPerLine + 1) + (col - 1)
      getDocument.remove(offset, 1)
      getDocument.insertString(offset, String.valueOf(c), null)

      if (col < charsPerLine) _cursorPos = (line, col + 1)
      else moveCursorToNextLine
    }
  }
  private def moveCursorToNextLine {
    val nextLine = math.min(numLines, _cursorPos._1 + 1)
    _cursorPos = (nextLine, 0)
  }

  def clear {
    getDocument.remove(0, getDocument.getLength)
    for (line <- 0 until totalLines) {
      for (col <- 0 until charsPerLine) {
        getDocument.insertString(getDocument.getLength,
                                 String.valueOf(buffer(line)(col)),
                                 null)
      }
      getDocument.insertString(getDocument.getLength, "\n", null)
    }
  }

  def reset {
    val currentSize: Dimension = getSize
    val fontMetrics = getGraphics.getFontMetrics(getFont)
    charsPerLine  = currentSize.width / fontMetrics.charWidth('0')
    totalLines    = currentSize.height / fontMetrics.getHeight
    printf("SCREEN SIZE: %d LINES %d COLS\n", totalLines, charsPerLine)
    buffer = new Array[Array[Char]](totalLines)
    for (line <- 0 until totalLines) {
      buffer(line) = new Array[Char](charsPerLine)
      for (col <- 0 until charsPerLine) buffer(line)(col) = ' '
    }
    clear
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

class TextBuffer(screenModel: SwingScreenModel)
extends JTextPane with KeyListener {
  setMargin(new java.awt.Insets(TextBuffer.MarginTop,
                                TextBuffer.MarginLeft,
                                TextBuffer.MarginBottom,
                                TextBuffer.MarginRight))
  addKeyListener(this)
  var builder = new StringBuilder
  var inputMode = TextInputMode.InputNone
  def isCharInputMode = inputMode == TextInputMode.ReadChar
  def isLineInputMode = inputMode == TextInputMode.ReadLine
  private var inputStart    = 0
  private var maxInputChars = 0

  def reset {
    inputMode     = TextInputMode.InputNone
    builder       = new StringBuilder
    inputStart    = 0
    maxInputChars = 0
    setText("")
  }

  def putChar(c: Char) {
    builder.append(c)
  }
  def flush {
    val doc = getDocument
    doc.insertString(doc.getLength, builder.toString, null)
    builder = new StringBuilder
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
        //printChar('\n')
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
    inputStart    = getDocument.getLength
    maxInputChars = maxChars
    inputMode     = TextInputMode.ReadLine
  }
  def requestCharInput {
    requestFocusInWindow
    getCaret.setVisible(true)
    inputMode = TextInputMode.ReadChar
  }
}

/*
 * Standard screen model for all versions except 6.
 */
class SwingScreenModel(topWindow: TextGrid) extends JPanel(new BorderLayout)
with OutputStream with InputStream with ScreenModel with FocusListener {
  var vm: Machine  = null
  var activeWindow = 0 // 0 is the bottom window, 1 is the top window
  val statusBar    = new StatusBar
  val mainPane     = new JPanel(new BorderLayout)
  val bottomWindow = new TextBuffer(this)
  val scrollPane   = new JScrollPane(bottomWindow)
  scrollPane.setPreferredSize(new Dimension(640, 480))
  mainPane.add(scrollPane, BorderLayout.CENTER)
  add(mainPane, BorderLayout.CENTER)
  topWindow.addFocusListener(this)

  def connect(aVm: Machine) {
    vm = aVm
    remove(statusBar)
    if (vm.version <= 3) {
      add(statusBar, BorderLayout.NORTH)
    }
  }

  private var selected: Boolean = true
  def isSelected = selected
  def select(flag: Boolean) = selected = flag
  def putChar(c: Char) {
    if (SwingUtilities.isEventDispatchThread) _putChar(c)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _putChar(c)
      })
    }
  }
  def _putChar(c: Char) {
    if (activeWindow == 0) bottomWindow.putChar(c)
    else topWindow.putChar(c)
  }
  def _flush {
    if (activeWindow == 0) bottomWindow.flush
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
    bottomWindow.requestLineInput(maxChars)
    0
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
    topWindow.windowSize = lines
    if (vm.version == 3) topWindow.clear
  }
  def setWindow(windowId: Int) {
    if (windowId == 0 || windowId == 1) {
      activeWindow = windowId
      printf("Setting active window to: %d\n", activeWindow)
    } else {
      throw new IllegalArgumentException(
        "@set_window illegal window: %d".format(windowId))
    }
  }
  def setCursor(line: Int, column: Int) {
    if (activeWindow == 1) {
      topWindow.cursorPos = (line, column)
    } else {
      throw new UnsupportedOperationException(
        "Can not set cursor in bottom window")
    }   
  }

  def bufferMode(flag: Int) {
    printf("@buffer_mode %d not implemented yet (TODO)\n", flag)
  }
  def eraseWindow(windowId: Int) {
    printf("@erase_window %d not implemented yet (TODO)\n", windowId)
  }
  def eraseLine(value: Int) {
    printf("@erase_line %d not implemented yet (TODO)\n", value)
  }
  def setTextStyle(style: Int) {
    printf("@set_text_style %d not implemented yet (TODO)\n", style)
    if (activeWindow == 0) {
      // set style in bottom window
    } else {
      // set style in top window
    }
  }

  def setColour(foreground: Int, background: Int, window: Int) {
    printf("@set_colour %d, %d, %d not implemented yet (TODO)\n",
           foreground, background, window)
  }

  def setFont(font: Int): Int = {
    printf("@set_font %d not implemented yet (TODO)\n", font)
    1
  }

  def initUI {
    println("INITIALIZE THE UI !!!")
    topWindow.setFont(new Font("Courier New", Font.PLAIN, 14))
    bottomWindow.setFont(new Font("American Typewriter", Font.PLAIN, 14))
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
}

class ZcodeFrame extends JFrame("ZMPP 2.0 Prototype") with WindowListener {
  val topWindow   = new TextGrid
  val screenModel = new SwingScreenModel(topWindow)
  var vm: Machine = null

  getContentPane.add(screenModel, BorderLayout.CENTER)
  getRootPane.getGlassPane.setVisible(true)
  getRootPane.setGlassPane(topWindow)
  pack
  addWindowListener(this)

  // WindowListener
  def windowOpened(event: WindowEvent) {
    // UI exists now, sizes can be initialized
    screenModel.initUI
    // make sure that the UI exists when execution is started
    ExecutionControl.executeTurn(vm, screenModel)
  }
  def windowActivated(event: WindowEvent) { }
  def windowDeactivated(event: WindowEvent) { }
  def windowDeiconified(event: WindowEvent) { }
  def windowIconified(event: WindowEvent) { }
  def windowClosed(event: WindowEvent) { }
  def windowClosing(event: WindowEvent) { }

  def runMachine(vm: Machine) {
    this.vm = vm
    screenModel.connect(vm)
    if (SwingUtilities.isEventDispatchThread) {
      setVisible(true)
    } else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = setVisible(true)
      })
    }
  }
}

object ExecutionControl {
  def _executeTurn(vm: Machine, screenModel: SwingScreenModel) {
    while (vm.state.runState == ZMachineRunStates.Running) {
      vm.doInstruction
    }
    if (vm.state.runState == ZMachineRunStates.ReadLine) {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = screenModel.readLine
      })
    } else if (vm.state.runState == ZMachineRunStates.ReadChar) {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = screenModel.readChar
      })
    }
  }

  def executeTurn(vm: Machine, screenModel: SwingScreenModel) {
    new Thread(new Runnable {
      def run = _executeTurn(vm, screenModel)
    }).start
  }
}

object ZcodeMain {
  def readFileData(file: File) = {
    val filebytes = new Array[Byte](file.length.toInt)
    var fileIs : FileInputStream = null
    try {
      fileIs = new FileInputStream(file)
      val numRead = fileIs.read(filebytes)
      if (numRead != file.length) {
        throw new java.io.IOException("FATAL: DID NOT READ ENOUGH BYTES")
      }
    } finally {
      if (fileIs != null) fileIs.close
    }
    filebytes
  }
  def readZcodeFile(file : File, screenModel: ScreenModel) = {
    val story = new DefaultMemory(readFileData(file))
    val vm = new Machine
    vm.init(story, screenModel)
    vm
  }
  
  def main(args: Array[String]) {
    val frame = new ZcodeFrame
    val vm = if (args.length == 0) {
      readZcodeFile(new File("minizork.z3"), frame.screenModel)
    } else {
      readZcodeFile(new File(args(0)), frame.screenModel)
    }
    frame.runMachine(vm)
  }
}
