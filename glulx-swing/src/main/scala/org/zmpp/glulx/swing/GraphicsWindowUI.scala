/*
 * Created on 2010/06/18
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
import java.awt.image.BufferedImage
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Color
import java.awt.event._

import org.zmpp.base._
import org.zmpp.glk._

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
  def _clear = {
    setBackground(new Color(_backgroundColor))
    _fillRect(_backgroundColor, 0, 0, getWidth, getHeight)
  }
  
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

  def _eraseRect(left: Int, top: Int, width: Int, height: Int) {
    val g2d = getOffscreenGraphics
    g2d.setColor(new java.awt.Color(_backgroundColor))
    g2d.fillRect(left, top, width, height)
    repaint()
  }
  override def eraseRect(left: Int, top: Int, width: Int, height: Int) {
    if (SwingUtilities.isEventDispatchThread) _eraseRect(left, top, width, height)
    else {
      SwingUtilities.invokeAndWait(new Runnable {
        def run = _eraseRect(left, top, width, height)
      })
    }
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
    logger.info("GRAPHICS WINDOW(%d), DRAWSCALEDIMAGE(%d)".format(glkWindow.id, resnum))
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
    logger.info("GRAPHICS WINDOW(%d), DRAWIMAGE(%d)".format(glkWindow.id, resnum))
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
    eventManager.addMouseEvent(glkWindow.id, xpos, ypos)
    waitForMouse = false
    if (screenUI.vm.state.runState == VMRunStates.WaitForEvent &&
      eventManager.processNextEvent) {
      ExecutionControl.executeTurn(screenUI.vm)   
    }
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

