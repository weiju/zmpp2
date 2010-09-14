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

import javax.swing._
import java.awt.event._
import java.awt._

import org.zmpp.base.Memory
import org.zmpp.base.DefaultMemory
import org.zmpp.base.VMRunStates

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
