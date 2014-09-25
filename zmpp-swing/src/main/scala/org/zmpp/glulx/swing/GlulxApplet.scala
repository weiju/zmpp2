/*
 * Created on 2010/05/01
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
package org.zmpp.glulx.swing

import java.awt.Dimension
import java.awt.Font
import javax.swing.JApplet
import java.net.URL
import org.zmpp.base._
import org.zmpp.iff._
import org.zmpp.glulx._

class GlulxApplet extends JApplet with SwingGlkScreenUI {
  val _vm = new GlulxVM

  private def readFromUrl(url: URL) {
    val conn = url.openConnection
    val numBytes = conn.getContentLength
    logger.info("Content-Length = " + numBytes)
    if (numBytes > 0) {
      val bytes = new Array[Byte](numBytes)
      val in = conn.getInputStream
      var offset = 0
      try {
        while (offset < numBytes) {
          val bytesRead = in.read(bytes, offset, numBytes - offset)
          offset += bytesRead
        }
      } catch {
        case e: Throwable => e.printStackTrace
      } finally {
        in.close
      }
      if (url.getFile.endsWith("ulx")) {
        _vm.init(bytes, null) 
      } else {
        // BLORB
        val iffdata = new DefaultMemory(bytes)
        val formchunk = new DefaultFormChunk(iffdata)
        val blorbData = new BlorbData(formchunk)
        val story = formchunk.chunkDataForId("GLUL")
        val storyBytes = new Array[Byte](story.size)
        story.copyBytesTo(storyBytes, 0, story.size)
        _vm.init(storyBytes, blorbData)
      }
    } else {
      logger.severe("NOT INITIALIZED (CONTENTLENGTH UNKNOWN)")
    }
  }
  
  def getFontFromParameter(paramName: String): Font = {
    val fontName = getParameter(paramName)
    val fontSize = getParameter(paramName + "-size")
    if (fontName != null && fontSize != null) {
      new Font(fontName, Font.PLAIN, Integer.parseInt(fontSize))
    } else null
  }

  override def init {
    val appletStdFont = getFontFromParameter("stdfont")
    val appletFixedFont = getFontFromParameter("fixedfont")
    if (appletStdFont != null) standardFont = appletStdFont
    if (appletFixedFont != null) fixedFont = appletFixedFont

    val storyUrl = new java.net.URL(getDocumentBase, getParameter("story-file"))
    readFromUrl(storyUrl)
    _vm.screenUI = this
    _vm.nativeSoundSystem = new JavaSeSoundSystem(_vm.blorbData, _vm)
    vm = _vm
    initMetrics
  }
  override def start {
    println("Start")
    ExecutionControl.executeTurn(vm)
  }

  def getClientSize = getContentPane.getSize
}

