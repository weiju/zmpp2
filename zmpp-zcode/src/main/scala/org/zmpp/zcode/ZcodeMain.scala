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

class StatusBar extends JLabel(" ") {
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

class TextBuffer(screenModel: SwingScreenModel) extends JTextPane with KeyListener {
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
  
  def printChar(c: Char) {
    builder.append(c)
  }
  def printNum(num: Int) {
    builder.append(num)
  }
  def flush {
    val doc = getDocument
    doc.insertString(doc.getLength, builder.toString, null)
    builder = new StringBuilder
  }
  
  // ****** KeyListener ******
  def keyPressed(event: KeyEvent) {
    if (isCharInputMode) {
      /*
      val keyCode = glkKeyCode(event.getKeyCode)
      if (keyCode != GlkKeyCodes.Unknown) {
        event.consume
        _screenUI.vm.resumeWithCharInput(glkWindow.id, keyCode)
        inputMode = SwingTextWindowUI.InputModeNone
        ExecutionControl.executeTurn(_screenUI.vm)
      }
      */
    } else if (isLineInputMode) {
      val doc = getDocument
      val caretPos = getCaret.getDot
      if (caretPos < inputStart) getCaret.setDot(doc.getLength)

      if (event.getKeyCode == KeyEvent.VK_ENTER) {
        event.consume
        val input = doc.getText(inputStart, doc.getLength - inputStart)
        //println("Input was: " + input)
        //printChar('\n')
        screenModel.resumeWithLineInput(input)          
        inputMode = TextInputMode.InputNone
        //ExecutionControl.executeTurn(_screenUI.vm)
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
      /*
      event.consume
      _screenUI.vm.resumeWithCharInput(glkWindow.id, event.getKeyChar.toInt)          
      inputMode = SwingTextWindowUI.InputModeNone
      ExecutionControl.executeTurn(_screenUI.vm)
      */
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

class SwingScreenModel extends JPanel(new BorderLayout)
with PlatformIO with OutputStream with InputStream {
  var vm: Machine = null
  val statusBar = new StatusBar
  val textbuffer = new TextBuffer(this)
  val scrollPane = new JScrollPane(textbuffer)
  textbuffer.setPreferredSize(new Dimension(640, 480))
  add(statusBar, BorderLayout.NORTH)
  add(scrollPane, BorderLayout.CENTER)

  def printChar(c: Char) {
    textbuffer.printChar(c)
  }
  def printNum(num: Int) {
    textbuffer.printNum(num)
  }
  def _flush {
    textbuffer.flush
  }
  def flush {
    if (SwingUtilities.isEventDispatchThread) _flush
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _flush
      })
    }
  }

  // input
  def readLine: Int = {
    val maxChars = vm.readLineInfo.maxInputChars
    println("MAX_CHARS FOR READLINE: " + maxChars)
    // update status line
    val objectName  = vm.statusLineObjectName
    val scoreOrTime = vm.statusLineScoreOrTime
    statusBar.setText(objectName + " " + scoreOrTime)
    textbuffer.requestLineInput(maxChars)
    0
  }
  def resumeWithLineInput(input: String) {
    println("RESUME WITH " + input)
    vm.resumeWithLineInput(input)
  }
  
  def screenOutputStream = this
  def keyboardStream = this
}

class ZcodeFrame extends JFrame("ZMPP 2.0 Prototype") {
  val screenModel = new SwingScreenModel
  getContentPane.add(screenModel, BorderLayout.CENTER)
  pack

}
object ZcodeMain {
  private var _vm : Machine = null
  private var _frame: ZcodeFrame = null

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
  def readZcodeFile(file : File) = {
    val story = new DefaultMemory(readFileData(file))
    _vm = new Machine
    _vm.init(story, _frame.screenModel)
    _frame.screenModel.vm = _vm
    _vm
  }
  
  def main(args: Array[String]) {
    _frame = new ZcodeFrame
    _frame.setVisible(true)

    val vm = readZcodeFile(new File("minizork.z3"))
    println("Zcode ZMPP")
    
    // do in thread
    //while (_vm.state) {
      _vm.doTurn
      // TODO: not only line input
      if (_vm.state.runState == VMRunStates.WaitForEvent) {
        _frame.screenModel.readLine
      }
    //}
  }
}

