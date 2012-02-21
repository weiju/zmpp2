/*
 * Created on 2010/04/09
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
package org.zmpp.glulx.swing

import java.util.logging._
import java.io.File

import javax.swing._
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Font
import java.awt.event._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import org.zmpp.base._
import org.zmpp.glk._
import org.zmpp.glk.io._
import org.zmpp.glulx._

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
  def eraseRect(left: Int, top: Int, width: Int, height: Int) {
    throw new UnsupportedOperationException(
      "fillRect() not supported for this window type")
  }
  def fillRect(color: Int, left: Int, top: Int, width: Int, height: Int) {
    throw new UnsupportedOperationException(
      "fillRect() not supported for this window type")
  }
  def drawScaledImage(resnum: Int, posx: Int, posy: Int, width: Int,
                      height: Int) {
    throw new UnsupportedOperationException(
      "This window does not support drawing images")
  }
  def drawImage(resnum: Int, posx: Int, posy: Int) {
    throw new UnsupportedOperationException(
      "This window does not support drawing images")
  }
  def requestLineInput
  def requestPreviousLineInput
  def requestCharInput
  def requestHyperlinkEvent
  def requestMouseInput
  def cancelLineInput: String
}

class Hyperlink(val id: Int) {
  var startPos: Int = 0
  var endPos  : Int = 0
  
  def contains(pos: Int): Boolean = pos >= startPos && pos <= endPos
}

trait SwingGlkScreenUI extends GlkScreenUI {
  val logger = Logger.getLogger("glk.ui")
  private[this] val _windowUIs = new HashMap[Int, SwingGlkWindowUI]
  private[this] val _imageCache = new HashMap[Int, BufferedImage]
  private[this] val TextGridExtraMargin = 3
  private[this] val TextBufferExtraMargin = 3
  var fixedFont = getDefaultFixedFont
  var standardFont = getDefaultNormalFont
  
  var lineHeightTextGrid   = 0
  var charWidthTextGrid    = 0
  var lineHeightStdFont    = 0
  var charWidthStdFont     = 0
  var vm: GlulxVM = null
  var currentView : JComponent = null

  // Currently, this "status bar" is just a focus catcher, i.e. it captures
  // the focus when the view layout hierarchy is reorganized
  var statusBar : JLabel = null
  def eventManager = vm.eventManager
  def blorbData   = vm.blorbData

  private def getDefaultFixedFont = {
    var prefFont = Font.decode("Inconsolata-PLAIN-14")
    if (prefFont.getFamily == "Dialog")
      prefFont = Font.decode("Courier New-PLAIN-14")
    if (prefFont.getFamily == "Dialog")
      prefFont = new Font("Monospaced", Font.PLAIN, 14)
    prefFont
  }
  private def getDefaultNormalFont = {
    var prefFont = Font.decode("American Typewriter-PLAIN-14")
    if (prefFont.getFamily == "Dialog")
      prefFont = Font.decode("Times New Roman-PLAIN-14")
    if (prefFont.getFamily == "Dialog")
      prefFont = new Font("Serif", Font.PLAIN, 14)
    prefFont
  }

  private def proportionalSize(fullSize: Int, relSize: Int): Int = {
    (fullSize.toDouble * relSize.toDouble / 100.0).toInt
  }

  private def fixedHeight(pair: GlkPairWindow) = {
    if (pair.keyWindow.isGraphics) pair.keyWindow.size
    else if (pair.keyWindow.isTextBuffer) {
      lineHeightStdFont * pair.keyWindow.size +
        SwingTextBufferUI.MarginTop + SwingTextBufferUI.MarginBottom
    }
    else {
      lineHeightTextGrid * pair.keyWindow.size +
        SwingTextGridUI.MarginTop + SwingTextGridUI.MarginBottom
    }
  }
  private def fixedWidth(pair: GlkPairWindow) = {
    if (pair.keyWindow.isGraphics) pair.keyWindow.size
    else if (pair.keyWindow.isTextBuffer) {
      charWidthStdFont * pair.keyWindow.size + SwingTextBufferUI.MarginLeft +
        SwingTextBufferUI.MarginRight
    }
    else {
      charWidthTextGrid * pair.keyWindow.size +
        SwingTextGridUI.MarginLeft + SwingTextGridUI.MarginRight
    }
  }

  private def calculateHeight(pair: GlkPairWindow, fullSize: Int): Int = {
    if (pair.isProportional) proportionalSize(fullSize, pair.keyWindow.size)
    else fixedHeight(pair)
  } 
  private def calculateWidth(pair: GlkPairWindow, fullSize: Int): Int = {
    if (pair.isProportional) proportionalSize(fullSize, pair.keyWindow.size)
    else fixedWidth(pair)
  } 

  private def distributeRemainder(window: GlkWindow, remainSize: Dimension) = {
    if (window.isLeaf) {
      // Leafs can always get the remainder size
      _windowUIs(window.id).asInstanceOf[JComponent]
        .setPreferredSize(remainSize)
      (_windowUIs(window.id).container, remainSize)
    } else {
      makeLayout(window, remainSize)
    }
  }

  private def makeLayout(window: GlkWindow,
                         currentSize: Dimension): JComponent = {
    if (window == null) {
      val emptyPanel = new JPanel
      emptyPanel.setPreferredSize(currentSize)
      emptyPanel
    } else if (window.isLeaf) {
      val component = _windowUIs(window.id).container
      component.setPreferredSize(currentSize)
      component
    } else {
      val pair = window.asInstanceOf[GlkPairWindow]
      var keyWidth  = 0
      var keyHeight = 0
      var leftSize: Dimension = null
      if (pair.isVertical) {
        keyWidth  = currentSize.width
        keyHeight = calculateHeight(pair, currentSize.height)
        leftSize  = new Dimension(currentSize.width,
                                  currentSize.height - keyHeight)
      } else {
        keyWidth  = calculateWidth(pair, currentSize.width)
        keyHeight = currentSize.height
        leftSize  = new Dimension(currentSize.width - keyWidth,
                                  currentSize.height)
      }
      val rightSize = new Dimension(keyWidth, keyHeight)
      val leftComponent  = makeLayout(pair.child0, leftSize)
      val rightComponent = makeLayout(pair.child1, rightSize)

      val pairPanel = if (pair.isVertical) new Box(BoxLayout.Y_AXIS)
        else new Box(BoxLayout.X_AXIS)

      if (pair.isLeft || pair.isAbove) {
        pairPanel.add(rightComponent)
        pairPanel.add(leftComponent)
        
      } else {
        pairPanel.add(leftComponent)
        pairPanel.add(rightComponent)
      }
      pairPanel
    }
  }
  
  def updateLayout(root: GlkWindow) {
    val runnable = new Runnable {
      def run {
        if (statusBar == null) {
          statusBar = new JLabel("")
          getContentPane.add(statusBar, BorderLayout.SOUTH)
        }
        val view = makeLayout(root, getClientSize)
        // ensure that we do not lose the input focus when we close the
        // current view. We do this by setting the input focus to the status
        // bar
        statusBar.requestFocusInWindow
        if (currentView != null) getContentPane.remove(currentView)
        getContentPane.invalidate
        getContentPane.add(view, BorderLayout.CENTER)
        currentView = view
        getContentPane.validate
        // we also need to reset the text grids in order
        // to pre-fill them with spaces
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
    val stdMetrics = g.getFontMetrics(standardFont)
    val fixedMetrics = g.getFontMetrics(fixedFont)
    lineHeightTextGrid = fixedMetrics.getMaxAscent + fixedMetrics.getMaxDescent
    charWidthTextGrid = g.getFontMetrics(fixedFont).charWidth('0')
    lineHeightStdFont = stdMetrics.getMaxAscent + stdMetrics.getMaxDescent
    charWidthStdFont = g.getFontMetrics(standardFont).charWidth('0')
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
  
  def _requestHyperlinkEvent(windowId: Int) {
    _windowUIs(windowId).flush
    _windowUIs(windowId).requestHyperlinkEvent
  }

  def requestHyperlinkEvent(windowId: Int) {
    if (SwingUtilities.isEventDispatchThread) _requestHyperlinkEvent(windowId)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _requestHyperlinkEvent(windowId)
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

  var _timer: javax.swing.Timer = null
  def requestTimerInput(millis: Int) {
    //logger.info("REQUESTING TIMER INPUT FOR %d MILLIS".format(millis))
    if (_timer != null) _timer.stop
    if (millis != 0) {
      _timer = new javax.swing.Timer(millis, new ActionListener {
        def actionPerformed(event: ActionEvent) {
          eventManager.addTimerEvent
          resumeWithNextEvent
        }
      })
      _timer.start
    }
  }
  
  private def resumeWithNextEvent {
    if (vm.runState == VMRunStates.WaitForEvent &&
        eventManager.processNextEvent) {
      ExecutionControl.executeTurn(vm)   
    }
  }

  def getContentPane: java.awt.Container
  def getGraphics: java.awt.Graphics
  def getClientSize: java.awt.Dimension
  
  def getImage(resnum: Int): BufferedImage = {
    //logger.info("getImage(%d)".format(resnum))
    val resourceInfo = blorbData.pictureResource(resnum)
    if (resourceInfo != null) {
      // many games use the same images over and over, cache them
      if (!_imageCache.contains(resnum)) {
        val inputStream = blorbData.pictureInputStream(resnum)
        _imageCache += resnum -> ImageIO.read(inputStream)
      }
      _imageCache(resnum)
    } else {
      logger.warning("IMAGE NUM NOT FOUND: %d".format(resnum))
      null
    }
  }
  
  def imageSize(resnum: Int): GlkDimension = {
    val image = getImage(resnum)
    if (image == null) null
    else new GlkDimension(image.getWidth, image.getHeight)
  }

  def selectFileByDialog(usage: Int, fmode: Int): File = {
    val usageType = usage & FileUsageTypes.TypeMask
    val fileTypeName = if (usageType == FileUsageTypes.SavedGame) "Game Save"
      else if (usageType == FileUsageTypes.Transcript) "Transcript"
      else if (usageType == FileUsageTypes.InputRecord) "Input Record"
      else "Data"

    val fileChooser = new JFileChooser
    val result = if (fmode == FileModes.Read) {
      fileChooser.setDialogTitle("Open %s File...".format(fileTypeName))
      fileChooser.showDialog(getContentPane, "Open")
    } else {
      fileChooser.setDialogTitle("Save %s File...".format(fileTypeName))
      fileChooser.showDialog(getContentPane, "Save")
    }
    if (result == JFileChooser.APPROVE_OPTION) fileChooser.getSelectedFile
    else null
  }
}

class GlkFrameUI extends JFrame with SwingGlkScreenUI {
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  def getClientSize = getContentPane.getSize
}
