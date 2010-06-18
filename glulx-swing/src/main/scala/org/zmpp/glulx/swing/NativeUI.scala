/*
 * Created on 2010/04/09
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

import java.io.InputStream
import java.util.logging._
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Font
import java.awt.Color
import java.awt.event._
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage
import javax.swing._
import java.awt.BorderLayout
import javax.swing.text.StyleConstants
import javax.swing.text.StyledDocument

import org.zmpp.base._
import org.zmpp.iff._
import org.zmpp.glulx._
import org.zmpp.glk._
import scala.collection.mutable.HashMap

/**
 * This trait ensures that we always run the UI relevant methods in the
 * event dispatch thread.
 */
trait SwingGlkWindowUI extends GlkWindowUI
with MouseListener with MouseMotionListener {
  def container: JComponent
  
  // functions that run in the UI thread
  def _moveCursor(xpos: Int, ypos: Int)
  def _setStyle(value: Int) {}
  def _flush {}
  def _clear
  def eventManager: EventManager

  def style = 0
  def style_=(value: Int) {
    if (SwingUtilities.isEventDispatchThread) _setStyle(value)
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _setStyle(value)
      })
    }
  }
  def flush {
    if (SwingUtilities.isEventDispatchThread) _flush
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _flush
      })
    }
  }
  def clear {
    if (SwingUtilities.isEventDispatchThread) _clear
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _clear
      })
    }
  }
  
  def moveCursor(x: Int, y: Int) {
    if (SwingUtilities.isEventDispatchThread) _moveCursor(x, y)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _moveCursor(x, y)
      })
    }
  }
  def fillRect(color: Int, left: Int, top: Int, width: Int, height: Int) {
    throw new UnsupportedOperationException("fillRect() not supported for this window type")
  }
  def setBackgroundColor(color: Int) {
    throw new UnsupportedOperationException("setBackground() not supported for this window type")
  }
  def drawScaledImage(resnum: Int, posx: Int, posy: Int, width: Int, height: Int) {
    throw new UnsupportedOperationException("This window does not support drawing images")
  }
  def drawImage(resnum: Int, posx: Int, posy: Int) {
    throw new UnsupportedOperationException("This window does not support drawing images")
  }
  def requestLineInput
  def requestPreviousLineInput
  def requestCharInput
  def requestMouseInput
  def cancelLineInput: String
}

class HyperLink(val id: Int) {
  var startPos: Int = 0
  var endPos  : Int = 0
}

object SwingTextWindowUI {
  val InputModeNone = 0
  val InputModeLine = 1
  val InputModeChar = 2
}
abstract class SwingTextWindowUI(val screenUI: SwingGlkScreenUI,
                                 val glkWindow: GlkUIWindow)
extends JTextPane with SwingGlkWindowUI with KeyListener {
  val logger = Logger.getLogger("glk.ui")
  var textInputMode = SwingTextWindowUI.InputModeNone
  var inputStart = 0
  var currentBackgroundColor = 0x00ffffff
  var currentForegroundColor = 0x00000000
  
  // a map of hyperlinks, cleared when the screen is cleared
  val hyperLinkMap = new HashMap[Int, HyperLink]
  var currentHyperLink: HyperLink = null
  
  addKeyListener(this)
  addMouseListener(this)
  addMouseMotionListener(this)

  protected def numCols: Int
  protected def numRows: Int
  def eventManager = screenUI.vm.eventManager
  
  /** Always return a smaller width to Glk to avoid formatting issues */
  def glkSize = new GlkDimension(numCols - 1, numRows)
  def glkSize_=(size: GlkDimension) {
    throw new UnsupportedOperationException("Setting text buffer window size not supported")
  }
  
  def isLineInputMode = textInputMode == SwingTextWindowUI.InputModeLine
  def isCharInputMode = textInputMode == SwingTextWindowUI.InputModeChar

  protected def resumeWithLineInput(input: String) {
    style = StyleType.Normal.id
    eventManager.resumeWithLineInput(glkWindow.id, input)          
    textInputMode = SwingTextWindowUI.InputModeNone
    ExecutionControl.executeTurn(screenUI.vm)
  }
  
  protected def resumeWithCharInput(c: Char) {
    eventManager.resumeWithCharInput(glkWindow.id, c.toInt)          
    textInputMode = SwingTextWindowUI.InputModeNone
    ExecutionControl.executeTurn(screenUI.vm)
  }

  def keyPressed(event: KeyEvent) {
    if (isCharInputMode) {
      val keyCode = glkKeyCode(event.getKeyCode)
      if (keyCode != GlkKeyCodes.Unknown) {
        event.consume
        eventManager.resumeWithCharInput(glkWindow.id, keyCode)
        textInputMode = SwingTextWindowUI.InputModeNone
        ExecutionControl.executeTurn(screenUI.vm)
      }
    } else if (isLineInputMode) {
      val doc = getDocument
      val caretPos = getCaret.getDot
      if (caretPos < inputStart) getCaret.setDot(doc.getLength)

      if (event.getKeyCode == KeyEvent.VK_ENTER) {
        event.consume
        val input = doc.getText(inputStart, doc.getLength - inputStart)
        logger.info("Input was: " + input)
        putChar('\n')
        resumeWithLineInput(input)
      } else if (event.getKeyCode == KeyEvent.VK_BACK_SPACE ||
                 event.getKeyCode == KeyEvent.VK_LEFT) {
        if (getCaret.getDot <= inputStart) {
          event.consume
        }
        // make sure that input style is not "removed".
        // Note: This sometimes works and sometimes not, why is that ?
        style = StyleType.Input.id
      } else if (event.getKeyCode == KeyEvent.VK_UP) {
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
      resumeWithCharInput(event.getKeyChar)
    } else if (!isLineInputMode) {
      // not in input mode, eat all key events
      event.consume
    }
  }
  def keyReleased(event: KeyEvent) {}
  
  // has to be called in UI event thread
  def requestLineInput {
    style = StyleType.Input.id
    requestFocusInWindow
    getCaret.setVisible(true)
    inputStart = getDocument.getLength
    textInputMode = SwingTextWindowUI.InputModeLine
  }
  def requestPreviousLineInput {
    style = StyleType.Input.id
    requestFocusInWindow
    getCaret.setVisible(true)
    textInputMode = SwingTextWindowUI.InputModeLine
  }
  def requestCharInput {
    requestFocusInWindow
    getCaret.setVisible(true)
    textInputMode = SwingTextWindowUI.InputModeChar
  }
  
  var _incompleteInput: String = null
  def _cancelLineInput {
    val doc = getDocument
    _incompleteInput = doc.getText(inputStart, doc.getLength - inputStart)
    printf("Canceled with input: '%s'\n", _incompleteInput)
  }

  def cancelLineInput: String = {
    if (SwingUtilities.isEventDispatchThread) _cancelLineInput
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _cancelLineInput
      })
    }
    _incompleteInput
  }

  def _setHyperlink(linkval: Int)
  
  def setHyperlink(linkval: Int) {
    if (SwingUtilities.isEventDispatchThread) _setHyperlink(linkval)
    else {
      SwingUtilities.invokeLater(new Runnable {
        def run = _setHyperlink(linkval)
      })
    }
  }

  private def glkKeyCode(keyCode: Int): Int = keyCode match {
    case KeyEvent.VK_LEFT      => GlkKeyCodes.Left
    case KeyEvent.VK_RIGHT     => GlkKeyCodes.Right
    case KeyEvent.VK_UP        => GlkKeyCodes.Up
    case KeyEvent.VK_DOWN      => GlkKeyCodes.Down
    case KeyEvent.VK_ENTER     => GlkKeyCodes.Return
    case KeyEvent.VK_DELETE    => GlkKeyCodes.Delete
    case KeyEvent.VK_ESCAPE    => GlkKeyCodes.Escape
    case KeyEvent.VK_TAB       => GlkKeyCodes.Tab
    case KeyEvent.VK_PAGE_UP   => GlkKeyCodes.PageUp
    case KeyEvent.VK_PAGE_DOWN => GlkKeyCodes.PageDown
    case KeyEvent.VK_HOME      => GlkKeyCodes.Home
    case KeyEvent.VK_END       => GlkKeyCodes.End
    case KeyEvent.VK_F1        => GlkKeyCodes.Func1
    case KeyEvent.VK_F2        => GlkKeyCodes.Func2
    case KeyEvent.VK_F3        => GlkKeyCodes.Func3
    case KeyEvent.VK_F4        => GlkKeyCodes.Func4
    case KeyEvent.VK_F5        => GlkKeyCodes.Func5
    case KeyEvent.VK_F6        => GlkKeyCodes.Func6
    case KeyEvent.VK_F7        => GlkKeyCodes.Func7
    case KeyEvent.VK_F8        => GlkKeyCodes.Func8
    case KeyEvent.VK_F9        => GlkKeyCodes.Func9
    case KeyEvent.VK_F10       => GlkKeyCodes.Func10
    case KeyEvent.VK_F11       => GlkKeyCodes.Func11
    case KeyEvent.VK_F12       => GlkKeyCodes.Func12
    case _                     => GlkKeyCodes.Unknown
  }
  
  def mouseMoved(event: MouseEvent) { }
  def mouseDragged(event: MouseEvent) { }
  def mouseExited(event: MouseEvent) { }
  def mouseEntered(event: MouseEvent) { }
  def mouseReleased(event: MouseEvent) { }
  def mousePressed(event: MouseEvent) { }
  def mouseClicked(event: MouseEvent) { }
}

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
  val attrs = getInputAttributes
  setFont(screenUI.standardFont)
  StyleConstants.setFontFamily(attrs, screenUI.standardFont.getFamily)
  StyleConstants.setFontSize(attrs,   screenUI.standardFont.getSize)

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
    setBackground(new Color(currentBackgroundColor))
    setForeground(new Color(currentForegroundColor))
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
  def _setHyperlink(linkval: Int) {
    throw new UnsupportedOperationException("no hyperlinks in text buffers")
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
  override protected def resumeWithCharInput(c: Char) {
    super.resumeWithCharInput(c)
    waitForMouse = false
  }
  private def resumeWithMouseInput(xpos: Int, ypos: Int) {
    eventManager.resumeWithMouseInput(glkWindow.id, xpos, ypos)          
    textInputMode = SwingTextWindowUI.InputModeNone
    waitForMouse  = false
    ExecutionControl.executeTurn(screenUI.vm)
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

/*
 * A Graphics window. We render the graphics into a backing store, that we
 * repaint
 */
class SwingGraphicsUI(screenUI: SwingGlkScreenUI, glkWindow: GlkWindow)
extends JComponent with SwingGlkWindowUI {
  val logger = Logger.getLogger("glk.ui")
  private var _backgroundColor = 0xffffffff
  private var waitForMouse  = false
  var buffer: BufferedImage = null
  var offscreenG2d: Graphics2D = null

  addMouseListener(this)
  addMouseMotionListener(this)

  def eventManager = screenUI.vm.eventManager
  def container = this
  def glkSize = new GlkDimension(getWidth, getHeight)
  def glkSize_=(size: GlkDimension) {
    throw new UnsupportedOperationException("Setting Graphics window size not supported")
  }

  // Stream methods that are not implemented
  def putChar(c: Char) { }
  def putCharUni(c: Int) { }

  var currentStyle = 0
  def _moveCursor(xpos: Int, ypos: Int) { }
  override def paintComponent(g:Graphics) {
    super.paintComponent(g)
    g.drawImage(buffer, 0, 0, null)
  }
  def _clear = _fillRect(_backgroundColor, 0, 0, getWidth, getHeight)
  
  private def getOffscreenGraphics: Graphics2D = {
    if (buffer == null) {
      //logger.info("Make OFFSCREEN BUFFER W = %d H = %d".format(getWidth, getHeight))
      buffer = new BufferedImage(getWidth, getHeight, BufferedImage.TYPE_INT_ARGB)
      offscreenG2d = buffer.createGraphics
      offscreenG2d.setColor(Color.WHITE)
      offscreenG2d.fillRect(0, 0, getWidth, getHeight)
    }
    offscreenG2d
  }
  
  def _fillRect(color: Int, left: Int, top: Int, width: Int, height: Int) {
    val g2d = getOffscreenGraphics
    g2d.setColor(new java.awt.Color(color))
    g2d.fillRect(left, top, width, height)
    repaint()
  }
  override def fillRect(color: Int, left: Int, top: Int, width: Int, height: Int) {
    if (SwingUtilities.isEventDispatchThread) _fillRect(color, left, top, width, height)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _fillRect(color, left, top, width, height)
      })
    }
  }
  def _drawScaledImage(resnum: Int, posx: Int, posy: Int,
                       width: Int, height: Int) {
    val g2d = getOffscreenGraphics
    val image = screenUI.getImage(resnum).getScaledInstance(
      width, height, java.awt.Image.SCALE_SMOOTH)
    g2d.drawImage(image, posx, posy, null)
    repaint()
  }
  override def drawScaledImage(resnum: Int, posx: Int, posy: Int,
                               width: Int, height: Int) {
    if (SwingUtilities.isEventDispatchThread) _drawScaledImage(resnum, posx, posy, width, height)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _drawScaledImage(resnum, posx, posy, width, height)
      })
    }
  }
  def _drawImage(resnum: Int, posx: Int, posy: Int) {
    val g2d = getOffscreenGraphics
    val image = screenUI.getImage(resnum)
    g2d.drawImage(image, posx, posy, null)
    repaint()
  }
  override def drawImage(resnum: Int, posx: Int, posy: Int) {
    if (SwingUtilities.isEventDispatchThread) _drawImage(resnum, posx, posy)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _drawImage(resnum, posx, posy)
      })
    }
  }
  override def setBackgroundColor(color: Int) { _backgroundColor = color }
  def requestLineInput { }
  def requestPreviousLineInput { }
  def requestCharInput {
    throw new UnsupportedOperationException("NO REQUEST CHAR INPUT IN GFX WIN")
  }
  def requestMouseInput {
    waitForMouse = true
  }
  def setHyperlink(linkval: Int) {
    throw new UnsupportedOperationException("SET HYPERLINK NOT SUPPORTED")
  }

  private def resumeWithMouseInput(xpos: Int, ypos: Int) {
    eventManager.resumeWithMouseInput(glkWindow.id, xpos, ypos)
    waitForMouse = false
    ExecutionControl.executeTurn(screenUI.vm)
  }

  def mouseMoved(event: MouseEvent) { }
  def mouseDragged(event: MouseEvent) { }
  def mouseExited(event: MouseEvent) { }
  def mouseEntered(event: MouseEvent) { }
  def mouseReleased(event: MouseEvent) { }
  def mousePressed(event: MouseEvent) { }
  def mouseClicked(event: MouseEvent) {
    if (waitForMouse) {
      resumeWithMouseInput(event.getX(), event.getY())
    }
  }
  def cancelLineInput: String = {
    throw new UnsupportedOperationException("cancel line input not supported in graphics windows")
  }
}

/**
 * Events that are not directly user triggered and can be retrieved from
 * select_poll():
 * - Timer
 * - Rearrange
 * - SoundNotify
 */
class PollEvents(var timerEventAt: Long, var rearrangeEventAt: Long,
                 var soundNotifyAt: Long)

trait SwingGlkScreenUI extends GlkScreenUI {
  val logger = Logger.getLogger("glk.ui")
  private val _windowUIs = new HashMap[Int, SwingGlkWindowUI]
  private val _fixedFont = getDefaultFixedFont
  private val _stdFont = getDefaultNormalFont
  private val TextGridExtraMargin = 3
  private val _pollEvents = new PollEvents(0L, 0L, 0L)
  var lineHeightTextGrid   = 0
  var charWidthTextGrid    = 0
  var lineHeightStdFont    = 0
  var charWidthStdFont     = 0
  var vm: GlulxVM = null
  
  def eventManager = vm.eventManager
  def blorbData   = vm.blorbData
  def fixedFont    = _fixedFont
  def standardFont = _stdFont

  private def getDefaultFixedFont = {
    var prefFont = Font.decode("Inconsolata-PLAIN-14")
    //println("GOT FONT: " + prefFont.getFamily)
    if (prefFont.getFamily == "Dialog") prefFont = Font.decode("Courier New-PLAIN-14")
    //println("GOT FONT: " + prefFont.getFamily)
    if (prefFont.getFamily == "Dialog") prefFont = new Font("Monospaced", Font.PLAIN, 14)
    prefFont
  }
  private def getDefaultNormalFont = {
    var prefFont = Font.decode("American Typewriter-PLAIN-14")
    //println("GOT FONT: " + prefFont.getFamily)
    if (prefFont.getFamily == "Dialog") prefFont = Font.decode("Times New Roman-PLAIN-14")
    //println("GOT FONT: " + prefFont.getFamily)
    if (prefFont.getFamily == "Dialog") prefFont = new Font("Serif", Font.PLAIN, 14)
    prefFont
  }

  private def proportionalSize(full: Int, size: Int): Int = {
    (full.toDouble * size.toDouble / 100.0).toInt
  }
  private def fixedHeight(node: GlkLayoutTreeNode) = {
    if (node.isGraphics) node.size
    else lineHeightTextGrid * node.size + TextGridExtraMargin
  }
  private def fixedWidth(node: GlkLayoutTreeNode) = {
    if (node.isGraphics) node.size
    else charWidthTextGrid * node.size
  }

  private def calculateHeight(full: Int, node: GlkLayoutTreeNode): Int = {
    if (node.parent.isProportional) proportionalSize(full, node.size)
    else fixedHeight(node)
  } 
  private def calculateWidth(full: Int, node: GlkLayoutTreeNode): Int = {
    if (node.parent.isProportional) proportionalSize(full, node.size)
    else fixedWidth(node)
  } 

  private def distributeRemainder(node: GlkLayoutTreeNode, remainSize: Dimension) = {
    if (node.isLeaf) {
      // Leafs can always get the remainder size
      _windowUIs(node.id).asInstanceOf[JComponent].setPreferredSize(remainSize)
      (_windowUIs(node.id).container, remainSize)
    } else {
      makeLayout(node, remainSize)
    }
  }

  private def makeLayout(node: GlkLayoutTreeNode,
                         currentSize: Dimension): (JComponent, Dimension) = {
    if (node == null) {
      val emptyPanel = new JPanel
      emptyPanel.setPreferredSize(currentSize)
      (emptyPanel, currentSize)
    } else if (node.isLeaf) {
      val componentSize =
        if (node.parent != null && node.parent.isVertical) {
          new Dimension(currentSize.width,
                        calculateHeight(currentSize.height, node))                                   
        } else if (node.parent != null && node.parent.isHorizontal) {
          new Dimension(calculateWidth(currentSize.width, node),
                        currentSize.height)
        } else currentSize
      //logger.info("At child: [%s], size = (%d, %d)".format(_windowUIs(node.id).getClass.getName,
      //      componentSize.width, componentSize.height))
      (_windowUIs(node.id).container, componentSize)
    } else {
      //logger.info("makeLayout at pair = %d, child0 = %d, child1 = %d [w = %d h = %d]".format(
      //            node.id, node.child0.id, node.child1.id, currentSize.width, currentSize.height))
      val rightSide = makeLayout(node.child1, currentSize)
      val rightSize = rightSide._2

      if (node.child1.isLeaf) {
        // Right side leafs always have the correct size
        _windowUIs(node.child1.id).asInstanceOf[JComponent].setPreferredSize(rightSize)
      }
      
      val remainderSize = if (node.isHorizontal) {
        new Dimension(currentSize.width - rightSize.width, currentSize.height)
      } else {
        new Dimension(currentSize.width, currentSize.height - rightSize.height)
      }
      // distribute the remainder side to the left subtree
      val leftSide = distributeRemainder(node.child0, remainderSize)

//      val leftSide = makeLayout(node.child0, remainderSize)
      val leftSize = leftSide._2
      val pairPanel = if (node.isVertical) new Box(BoxLayout.Y_AXIS)
        else new Box(BoxLayout.X_AXIS)

      if (node.isLeft || node.isAbove) {
        pairPanel.add(rightSide._1)
        pairPanel.add(leftSide._1)
        val dir = if (node.isLeft) "LEFT" else "ABOVE"
        // Here we need to go down the tree and set the leaf node sizes
        /*
        logger.info("PAIR(%s) LEFT SIZE = (%d, %d) RIGHT SIZE: (%d, %d) REMAIN SIZE: (%d, %d)".format(
                    dir,
                    leftSize.width, leftSize.height,
                    rightSize.width, rightSize.height,
                    remainderSize.width, remainderSize.height))*/
        
      } else {
        pairPanel.add(leftSide._1)
        pairPanel.add(rightSide._1)
        val dir = if (node.isRight) "RIGHT" else "BELOW"
        // Here we need to go down the tree and set the leaf node sizes
        /*
        logger.info("PAIR(%s) LEFT SIZE = (%d, %d) RIGHT SIZE: (%d, %d) REMAIN SIZE: (%d, %d)".format(
                    dir,
                    leftSize.width, leftSize.height,
                    rightSize.width, rightSize.height,
                    remainderSize.width, remainderSize.height))*/
      }
      if (node.isVertical) {
        (pairPanel, new Dimension(remainderSize.width,
                                  leftSize.height + rightSize.height))
      } else {
        (pairPanel, new Dimension(leftSize.width + rightSize.width,
                                  remainderSize.height))
      }
    }
  }
  
  def updateLayout(root: GlkLayoutTreeNode) {
    //logger.info("UPDATE LAYOUT")
    val runnable = new Runnable {
      def run {
        val viewAndSize = makeLayout(root, getSize)
        //logger.info("ADDING A: " + viewAndSize._1)
        getContentPane.removeAll
        getContentPane.invalidate
        getContentPane.add(viewAndSize._1, BorderLayout.CENTER)
        getContentPane.validate
        _windowUIs.foreach(elem => if (elem._2.isInstanceOf[SwingTextGridUI]) {
          elem._2.asInstanceOf[SwingTextGridUI].reset
        })
      }
    }
    // resize synchronously to make sure the VM can retrieve the right size
    // (we might do synchronous getsize instead)
    if (SwingUtilities.isEventDispatchThread) runnable.run
    else SwingUtilities.invokeAndWait(runnable)
  }

  def createTextBufferUI(id: Int, glkWindow: GlkUIWindow) = {
    val winui = new SwingTextBufferUI(this, glkWindow)
    winui.setPreferredSize(new Dimension(640, 480))
    _windowUIs += id -> winui
    winui
  }
  def createTextGridUI(id: Int, glkWindow: GlkUIWindow) = {
    val winui = new SwingTextGridUI(this, glkWindow)
    winui.setPreferredSize(new Dimension(640, lineHeightTextGrid))
    _windowUIs += id -> winui
    winui
  }
  def createGraphicsUI(id: Int, glkWindow: GlkWindow) = {
    val winui = new SwingGraphicsUI(this, glkWindow)
    winui.setPreferredSize(new Dimension(200, 10))
    _windowUIs += id -> winui
    winui
  }
  
  def initMetrics {
    val g =  getGraphics
    lineHeightTextGrid = g.getFontMetrics(_fixedFont).getHeight
    charWidthTextGrid = g.getFontMetrics(_fixedFont).charWidth('0')
    lineHeightStdFont = g.getFontMetrics(_stdFont).getHeight
    charWidthStdFont = g.getFontMetrics(_stdFont).charWidth('0')
  }
  
  // Line input
  def _requestLineInput(windowId: Int) {
    //logger.info("REQUEST_LINE_INPUT(%d)".format(windowId))
    val windowUI = _windowUIs(windowId)
    windowUI.flush
    _windowUIs(windowId).requestLineInput
  }
  
  def requestLineInput(windowId: Int) {
    if (SwingUtilities.isEventDispatchThread) _requestLineInput(windowId)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _requestLineInput(windowId)
      })
    }
  }

  def _requestPreviousLineInput(windowId: Int) {
    val windowUI = _windowUIs(windowId)
    windowUI.flush
    _windowUIs(windowId).requestPreviousLineInput
  }
  
  def requestPreviousLineInput(windowId: Int) {
    if (SwingUtilities.isEventDispatchThread) _requestPreviousLineInput(windowId)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _requestPreviousLineInput(windowId)
      })
    }
  }

  // Character input
  def _requestCharInput(windowId: Int) {
    //logger.info("LISTEN TO CHAR_INPUT(%d)".format(windowId))
    _windowUIs(windowId).flush
    _windowUIs(windowId).requestCharInput
  }

  def requestCharInput(windowId: Int) {
    if (SwingUtilities.isEventDispatchThread) _requestCharInput(windowId)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _requestCharInput(windowId)
      })
    }
  }
  
  // Mouse input
  def _requestMouseInput(windowId: Int) {
    //logger.info("LISTEN TO MOUSE_INPUT(%d)".format(windowId))
    _windowUIs(windowId).flush
    _windowUIs(windowId).requestMouseInput
  }
  def requestMouseInput(windowId: Int) {
    if (SwingUtilities.isEventDispatchThread) _requestMouseInput(windowId)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _requestMouseInput(windowId)
      })
    }
  }
  
  def cancelLineInput(windowId: Int): String = {
    _windowUIs(windowId).cancelLineInput
  }

  
  def pollEvents = {
    if (_pollEvents.timerEventAt != 0) {
      _pollEvents.timerEventAt = 0L
      GlkEventType.Timer
    }
    else GlkEventType.EventNone
  }
  
  var _timer: javax.swing.Timer = null
  def requestTimerInput(millis: Int) {
    logger.info("REQUESTING TIMER INPUT FOR %d MILLIS".format(millis))
    if (_timer != null) _timer.stop
    if (millis != 0) {
      _timer = new javax.swing.Timer(millis, new ActionListener {
        def actionPerformed(event: ActionEvent) {
          val timestamp = System.currentTimeMillis
          if (_pollEvents.timerEventAt == 0) {
            _pollEvents.timerEventAt = timestamp
          }
          if (vm.state.runState == VMRunStates.WaitForEvent &&
              _pollEvents.timerEventAt != 0) {
            _pollEvents.timerEventAt = 0
            eventManager.resumeWithTimerEvent
            ExecutionControl.executeTurn(vm)
          }
        }
      })
      _timer.start
    }
  }

  def getContentPane: java.awt.Container
  def getGraphics: java.awt.Graphics
  def getSize: java.awt.Dimension
  
  // TODO: Cache images
  def getImage(resnum: Int): BufferedImage = {
    //logger.info("getImage(%d)".format(resnum))
    val resourceInfo = blorbData.pictureResource(resnum)
    if (resourceInfo != null) {
      val inputStream = blorbData.pictureInputStream(resnum)
      ImageIO.read(inputStream)
    } else null
  }
  
  def imageSize(resnum: Int): GlkDimension = {
    val image = getImage(resnum)
    if (image == null) null
    else new GlkDimension(image.getWidth, image.getHeight)
  }
}

class GlkFrameUI extends JFrame with SwingGlkScreenUI


