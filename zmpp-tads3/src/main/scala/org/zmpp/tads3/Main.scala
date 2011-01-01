/*
 * Created on 2010/05/05
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
package org.zmpp.tads3

import java.util.logging._
import java.io.File
import java.io.FileInputStream

import javax.swing._
import java.awt._
import java.awt.event._

import org.zmpp.base.Types
import org.zmpp.base.Memory
import org.zmpp.base.DefaultMemory

trait TadsOutput {
  def addString(str: String)
}

class TadsVM {
  val objectSystem           = new ObjectSystem
  val functionSetMapper      = new IntrinsicFunctionSetMapper
  val vmState                = new TadsVMState(objectSystem, functionSetMapper)
  var currentExecutor: Executor = null

  def init(imageMem: Memory, tadsOutput: TadsOutput) {
    vmState.reset(imageMem, tadsOutput)
    currentExecutor = new Executor(vmState)
    printf("VALID FILE: %b, Version: %d Timestamp: %s\n",
           vmState.image.isValid, vmState.image.version, vmState.image.timestamp)
  }
  def doTurn = currentExecutor.doTurn
}


class TadsFrame extends JFrame("ZMPP 2 (TADS 3)") with TadsOutput {
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  val textPane = new JTextPane
  val scrollPane = new JScrollPane(textPane)
  scrollPane.setPreferredSize(new Dimension(800, 600))
  getContentPane.add(scrollPane, BorderLayout.CENTER)
  pack
  setVisible(true)

  def addString(str: String) {
    textPane.getDocument.insertString(textPane.getDocument.getLength, str, null)
  }
}

object Tads3Main {
  private var _vm : TadsVM = null

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

  def readTads3File(file: File, tadsOutput: TadsOutput) = {
    // Little Endian format - Hello Intel-Lovers
    val imageMem     = new DefaultMemory(readFileData(file)).littleEndian
    _vm = new TadsVM
    _vm.init(imageMem, tadsOutput)
    _vm
  }

  def main(args: Array[String]) {
    println("ZMPP TADS3 (Prototype version)")
    val frame = new TadsFrame
    frame.addString("ZMPP 2 rocks !\n")

    val vm = readTads3File(new File(args(0)), frame)
    println("\nProgram:\n---------")
    vm.doTurn
  }
}
