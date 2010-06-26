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

import java.util.logging._

import javax.swing._
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Font
import java.awt.event._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import org.zmpp.base._
import org.zmpp.glk._
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

/**
 * Events that are not directly user triggered and can be retrieved from
 * select_poll():
 * - Timer
 * - Rearrange
 * - SoundNotify
 */
/*
class PollEvents(var timerEventAt: Long, var rearrangeEventAt: Long,
                 var soundNotifyAt: Long)
*/

trait SwingGlkScreenUI extends GlkScreenUI {
  val logger = Logger.getLogger("glk.ui")
  private val _windowUIs = new HashMap[Int, SwingGlkWindowUI]
  private val _fixedFont = getDefaultFixedFont
  private val _stdFont = getDefaultNormalFont
  private val TextGridExtraMargin = 3
  //private val _pollEvents = new PollEvents(0L, 0L, 0L)
  
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

  var _timer: javax.swing.Timer = null
  def requestTimerInput(millis: Int) {
    logger.info("REQUESTING TIMER INPUT FOR %d MILLIS".format(millis))
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
    if (vm.state.runState == VMRunStates.WaitForEvent &&
        eventManager.processNextEvent) {
      ExecutionControl.executeTurn(vm)   
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
    } else {
      logger.info("IMAGE NUM NOT FOUND: %d".format(resnum))
      null
    }
  }
  
  def imageSize(resnum: Int): GlkDimension = {
    val image = getImage(resnum)
    if (image == null) null
    else new GlkDimension(image.getWidth, image.getHeight)
  }
}

class GlkFrameUI extends JFrame with SwingGlkScreenUI


