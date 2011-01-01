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
import java.util.logging._
import java.awt.Dimension
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

import org.zmpp.base._
import org.zmpp.iff._
import org.zmpp.glulx._

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import javax.sound.sampled._
import javax.swing._
import javax.swing.table._
import java.awt._
import java.awt.event._

class ViewerTableModel extends AbstractTableModel {
  var _blorbData: BlorbData = null
  def blorbData = _blorbData
  def blorbData_=(aBlorbData: BlorbData) {
    _blorbData = aBlorbData
    fireTableDataChanged
  }
  def resources = blorbData.resources
  def getRowCount = if (blorbData == null) 0 else resources.length
  def getColumnCount = 5
  override def getColumnName(col: Int) = col match {
    case 0 => "Type"
    case 1 => "#"
    case 2 => "Subtype"
    case 3 => "Address"
    case 4 => "Size"
  }
  def getValueAt(row: Int, col: Int) = col match {
    case 0 =>
      resources(row).resourceType match {
        case ResourceTypes.Picture => "Picture"
        case ResourceTypes.Sound => "Sound"
        case ResourceTypes.Exec => "Exec"
        case _ => "?"
      }
    case 1 => resources(row).number.asInstanceOf[AnyRef]
    case 2 =>
      val subChunk = _blorbData.formChunk.chunkAtAddress(resources(row).start)
      val id = subChunk.id
      if (id == "FORM") "AIFF"
      else id
    case 3 => resources(row).start.asInstanceOf[AnyRef]
    case 4 =>
      val subChunk = _blorbData.formChunk.chunkAtAddress(resources(row).start)
      subChunk.size.asInstanceOf[AnyRef]
  }
}

class BlorbViewerFrame extends JFrame("Blorb Viewer") {
  val tableModel = new ViewerTableModel
  val viewerTable = new JTable(tableModel)
  val spane = new JScrollPane(viewerTable)
  val menubar = new JMenuBar
  var soundChannel: JavaSeSoundChannel = null

  val fileMenu = new JMenu("File")
  val openItem = new JMenuItem("Open...")
  val quitItem = new JMenuItem("Quit")
  fileMenu.add(openItem)
  fileMenu.add(quitItem)
  menubar.add(fileMenu)
  setJMenuBar(menubar)
  
  // tool bar
  val toolbar = new JToolBar
  val viewButton = new JButton("View")
  val saveResourceButton = new JButton("Save Resource")
  toolbar.add(viewButton)
  toolbar.add(saveResourceButton)
  
  getContentPane.add(toolbar, BorderLayout.NORTH)
  getContentPane.add(spane, BorderLayout.CENTER)
  pack
  
  val frame = this
  quitItem.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      System.exit(0)
    }
  })
  openItem.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val chooser = new JFileChooser
      if (chooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
        tableModel.blorbData = readGblorbFile(chooser.getSelectedFile)
        soundChannel = new JavaSeSoundChannel(tableModel.blorbData, null)
      }
    }
  })
  viewButton.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val selindex = viewerTable.getSelectedRow
      if (selindex >= 0 && selindex < tableModel.resources.length) {
        val resource = tableModel.resources(selindex)
        if (resource.isPicture) {
          println("PLAY PICTURE")
        } else if (resource.isSound) {
          println("PLAY SOUND")
          soundChannel.play(resource.number, 1, 0)
        }
      }
    }
  })
  saveResourceButton.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent) {
      val selindex = viewerTable.getSelectedRow
      if (selindex >= 0 && selindex < tableModel.resources.length) {
        val chooser = new JFileChooser
        if (chooser.showSaveDialog(frame) == JFileChooser.APPROVE_OPTION) {
          val resource = tableModel.resources(selindex)
          val blorbData = tableModel.blorbData
          val memory = tableModel.blorbData.formChunk.memory
          val subChunk = blorbData.formChunk.chunkAtAddress(resource.start)
          if (subChunk.id == "FORM") {
            val size = subChunk.size
            val data = new Array[Byte](size)
            memory.copyBytesTo(data, resource.start, size)
            val fos = new FileOutputStream(chooser.getSelectedFile)
            fos.write(data)
            fos.close
          } else {
            val size = subChunk.size
            println("SAVE RESOURCE")
            val data = new Array[Byte](size)
            memory.copyBytesTo(data, resource.start + Chunk.HeaderLength, size)
            val fos = new FileOutputStream(chooser.getSelectedFile)
            fos.write(data)
            fos.close
          }
        }        
      }
    }
  })

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

  def readGblorbFile(file: File) = {
    val iffdata = new DefaultMemory(readFileData(file))
    val formchunk = new DefaultFormChunk(iffdata)
    new BlorbData(formchunk)
  }
}


object BlorbViewer {
  def main(args: Array[String]) {
    val frame = new BlorbViewerFrame
    frame.setVisible(true)
  }
}

