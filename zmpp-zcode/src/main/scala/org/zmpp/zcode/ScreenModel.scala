/*
 * Created on 2010/08/07
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

object ScreenModelConstants {
  val FontNormal       = 1
  val FontPicture      = 2
  val FontCharacterGfx = 3
  val FontFixed        = 4

  val ColorCurrent     = 0
  val ColorDefault     = 1
  val ColorBlack       = 2
  val ColorRed         = 3
  val ColorGreen       = 4
  val ColorYellow      = 5
  val ColorBlue        = 6
  val ColorMagenta     = 7
  val ColorCyan        = 8
  val ColorWhite       = 9

  // these colors are V6 only
  val ColorGray1       = 10
  val ColorGray2       = 11
  val ColorGray3       = 12
  val ColorUnderCursor = -1
}

object WindowAttributes {
  val Wrapping         = 0
  val Scrolling        = 1
  val Transcript       = 2
  val Buffered         = 3
}

object WindowProperties {
  val CoordY           = 0
  val CoordX           = 1
  val SizeY            = 2
  val SizeX            = 3
  val CursorY          = 4
  val CursorX          = 5
  val MarginSizeLeft   = 6
  val MarginSizeRight  = 7
}

trait ScreenModel {
  def screenOutputStream : OutputStream
  def keyboardStream     : InputStream

  def updateStatusLine
  def splitWindow(lines: Int)
  def setWindow(windowId: Int)
  def setCursor(line: Int, column: Int)
  def connect(vm: Machine)
  def initUI // set sizes etc.

  def bufferMode(flag: Int)
  def eraseWindow(windowId: Int)
  def eraseLine(value: Int)
  def setTextStyle(style: Int)
  def setFont(font: Int): Int
  def setColour(foreground: Int, background: Int, window: Int)
}
