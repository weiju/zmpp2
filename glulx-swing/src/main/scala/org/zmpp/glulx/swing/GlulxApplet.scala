/*
 * Created on 2010/05/01
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
    println("Content-Length = " + numBytes)
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
        case e => e.printStackTrace
      } finally {
        in.close
      }
      if (url.getFile.endsWith("ulx")) {
        val story = new DefaultMemory(bytes)
        _vm.init(story, null) 
      } else {
        // BLORB
        val iffdata = new DefaultMemory(bytes)
        val formchunk = new DefaultFormChunk(iffdata)
        logger.info("FORM, sub id: " + formchunk.subId)
        val blorbData = new BlorbData(formchunk)
        val story = formchunk.chunkDataForId("GLUL")
        _vm.init(story, blorbData) 
      }
    } else {
      println("NOT INITIALIZED (CONTENTLENGTH UNKNOWN)")
    }
  }

  override def init {
    val storyUrl = new java.net.URL(getDocumentBase, getParameter("story-file"))
    readFromUrl(storyUrl)
    _vm.screenUI = this
    _vm.nativeSoundSystem = new JavaSeSoundSystem(_vm.blorbData)
    vm = _vm
    initMetrics
  }
  override def start {
    println("Start")
    ExecutionControl.executeTurn(vm)
  }
}

