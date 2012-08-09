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

import java.io._
import java.awt.Dimension
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JFileChooser

import org.zmpp.base._
import org.zmpp.iff._
import org.zmpp.glulx._
import org.zmpp.glk.windows._

object Glulx {
  private[this] var _vm : GlulxVM = null
  def readFileData(file: File) = {
    val filebytes = new Array[Byte](file.length.toInt)
    var fileIs : FileInputStream = null
    try {
      fileIs = new FileInputStream(file)
      fileIs.read(filebytes)
    } finally {
      if (fileIs != null) fileIs.close
    }
    filebytes
  }

  def readFile(filename: String) = {
    val file = new File(filename)
    if (filename.endsWith(".ulx")) readUlxFile(file)
    else readGblorbFile(file)
  }
  
  def readUlxFile(file : File) = {
    val storyBytes = readFileData(file)
    _vm = new GlulxVM
    _vm.init(storyBytes, null)
    _vm
  }
  def readGblorbFile(file: File) = {
    val iffdata = new DefaultMemory0(readFileData(file))
    val formchunk = new DefaultFormChunk(iffdata)
    val blorbData = new BlorbData(formchunk)
    val story = formchunk.chunkDataForId("GLUL")
    _vm = new GlulxVM
    val storyBytes = new Array[Byte](story.size)
    story.copyBytesTo(storyBytes, 0, story.size)
    _vm.init(storyBytes, blorbData)
    _vm
  }
}

object ExecutionControl {

  def executeTurn(vm: GlulxVM) {
    new Thread(new Runnable {
      def run = vm.executeTurn
    }).start
  }

  def runStory(screenUI: SwingGlkScreenUI, filename: String) {    
    try {
      val vm = Glulx.readFile(filename)
      vm.glk.screenUI = screenUI
      screenUI.vm = vm
      vm.glk.nativeSoundSystem = new JavaSeSoundSystem(vm.blorbData, vm)
      
      executeTurn(vm)
    } catch {
      case ex: Throwable => ex.printStackTrace
    }
  }  
}

object GlulxMain extends App {
  val filename = if (args.length == 0) {
    // FileChooser
    val fileChooser = new JFileChooser
    val result = fileChooser.showOpenDialog(null)
    if (result == JFileChooser.APPROVE_OPTION)
      fileChooser.getSelectedFile.getAbsolutePath
    else null
  } else args(0)
  val frame = new GlkFrameUI
  frame.addWindowListener(new WindowAdapter {
    override def windowOpened(evt: WindowEvent) {
      frame.initMetrics
      ExecutionControl.runStory(frame, filename)
    }
  })
  frame.setTitle("ZMPP Glulx 1.0")
  frame.setPreferredSize(new Dimension(640, 480))
  frame.pack
  frame.setVisible(true)
}

