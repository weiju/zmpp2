/*
 * Created on 2010/08/07
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
package org.zmpp.zcode

import java.util.LinkedList

// could be encoded in 3 bit
object Fonts {
  val Normal             = 1
  val Picture            = 2
  val CharacterGfx       = 3
  val Fixed              = 4
}

// could be encoded in 4 bit
object Colors {
  val Current            = 0
  val Default            = 1
  val Black              = 2
  val Red                = 3
  val Green              = 4
  val Yellow             = 5
  val Blue               = 6
  val Magenta            = 7
  val Cyan               = 8
  val White              = 9

  // these colors are V6 only
  val Gray1              = 10
  val Gray2              = 11
  val Gray3              = 12
  val UnderCursor        = -1
}

// could be encoded in 4 bit
object TextStyles {
  val Roman              = 0
  val ReverseVideo       = 1
  val Bold               = 2
  val Italic             = 4
  val FixedPitch         = 8

  def isRoman(style: Int) = (style & 0x0f) == Roman
  def isReverseVideo(style: Int) = (style & ReverseVideo) == ReverseVideo
  def isBold(style: Int) = (style & Bold)  == Bold
  def isItalic(style: Int) = (style & Italic) == Italic
  def isFixed(style: Int) = (style & FixedPitch) == FixedPitch

  val DefaultNormal = makeStyle(0, Fonts.Normal, Colors.Default, Colors.Default)
  val DefaultFixed  = makeStyle(0, Fonts.Fixed,  Colors.Default, Colors.Default)
  val DefaultFixedBlank = styleChar(' ', DefaultFixed)

  /*
   * encode style, font number and colors into a 16 bit value
   * mask: ffffbbbb  0fffssss
   */
  def makeStyle(styleMask: Int, fontnum: Int, foreground: Int,
                background: Int): Int = {
    ((foreground & 0x0f) << 12) | ((background & 0x0f) << 8) |
    ((fontnum & 0x07) << 4) | (styleMask & 0x0f)
  }
  def fontNumber(style: Int) = (style >>>  4) & 0x07
  def foregroundColor(style: Int) = (style >>>  12) & 0x0f
  def backgroundColor(style: Int) = (style >>>  8) & 0x0f
  def withStyle(style: Int, styleMask: Int): Int = {
    (style & 0xfff0) | styleMask
  }
  def withColors(style: Int, foreground: Int, background: Int) = {
    (style & 0x00ff) | ((foreground & 0x0f) << 12) |
    ((background & 0x0f) << 8)
  }
  def withFont(style: Int, fontnum: Int): Int = {
    (style & 0x00f0) | ((fontnum & 0x07) << 4)
  }
  def styleChar(c: Char, style: Int) = {
    ((c & 0xffff) << 16) | (style & 0xffff)
  }
}

object WindowAttributes {
  val Wrapping           = 0
  val Scrolling          = 1
  val Transcript         = 2
  val Buffered           = 3
}

object WindowProperties {
  val CoordY             = 0
  val CoordX             = 1
  val SizeY              = 2
  val SizeX              = 3
  val CursorY            = 4
  val CursorX            = 5
  val MarginSizeLeft     = 6
  val MarginSizeRight    = 7
  val InterruptRoutine   = 8
  val InterruptCountdown = 9
  val TextStyle          = 10
  val ColorData          = 11
  val FontNumber         = 12
  val FontSize           = 13
  val Attributes         = 14
  val LineCount          = 15
}

object ScreenModel {
  val BottomWindow  = 0
  val TopWindow     = 1
  val CurrentWindow = -3
}

trait ScreenModel {
  def screenOutputStream : OutputStream
  def keyboardStream     : InputStream
  def activeWindow       : ScreenModelWindow

  def updateStatusLine
  def splitWindow(lines: Int)
  def setWindow(windowId: Int)
  def setCursorPosition(line: Int, column: Int)
  def cursorPosition: (Int, Int)
  def connect(vm: Machine)
  def initUI // set sizes etc.

  def bufferMode(flag: Int)
  def eraseWindow(windowId: Int)
  def eraseLine(value: Int)
  def setTextStyle(style: Int)
  def setFont(font: Int): Int
  def setColour(foreground: Int, background: Int, window: Int)

  def capabilities: List[CapabilityFlag]
}

trait ScreenModelWindow {
  def reset: Unit
  def flush: Boolean
  def putChar(c: Char): Unit
  def clear: Unit
  def cursorPosition_=(pos: (Int, Int)): Unit
  def cursorPosition: (Int, Int)
}

case class StyledText(text: String, style: Int) {
  def isItalic = TextStyles.isItalic(style)
  def isBold = TextStyles.isBold(style)
  def isReverseVideo = TextStyles.isReverseVideo(style)
  def isFixed = TextStyles.isFixed(style)
  def foreground = TextStyles.foregroundColor(style)
  def background = TextStyles.backgroundColor(style)
}

/*
 * While text grids are not technically "buffered" according to the
 * specification, storing them before flushing them to the screen
 * comes with a lot of advantages:
 * - Game responsiveness appears to be much better when the top window is buffered
 * - Output can be easily clipped
 * - we can store and serialize the state
 * The grid buffer is represented as an array of 32bit values which
 * are basically the characters in the upper 16 bits and the style
 * in the lower. With this strategy, we can avoid creation of objects
 * and performance penalties due to garbage collection.
 */
class TextGridBuffer(numRows: Int, numColumns: Int) {
  import TextStyles._

  private val grid = Array.ofDim[Int](numRows, numColumns)
  fillGridWith(DefaultFixedBlank, 0)

  def fillGridWith(styledChar: Int, startRow: Int=0) {
    //printf("fillGridWith, c = '%c', startRow = %d\n", styledChar.c, startRow)
    var row = startRow
    var col = 0
    while (row < numRows) {
      col = 0
      while (col < numColumns) {
        grid(row)(col) = styledChar
        col += 1
      }
      row += 1
    }
  }

  def update(row: Int, column: Int, c: Int) {
    if (row >= 0 && row < numRows && column >= 0 && column < numColumns) {
      grid(row)(column) = c
    }
  }
  def apply(row: Int, column: Int) = grid(row)(column)
}

/**
 * A more sophisticated buffer to store text runs before they are
 * flushed. This is to increase the user response time by letting
 * the game thread run without waiting for the UI thread each time
 * a style change occurs.
 */
class TextRunBuffer {
  import TextStyles._
  private[this] var currentStyle = DefaultNormal
  private[this] var currentText: StringBuilder = new StringBuilder
  private[this] var runBuffer = new LinkedList[StyledText]()

  def reset {
    clear
    currentStyle = DefaultNormal
  }

  private def clear {
    currentText = new StringBuilder
    runBuffer = new LinkedList[StyledText]()
  }

  def append(char: Char) = currentText.append(char)
  def setStyle(styleMask: Int) {
    applyCurrentStyle
    currentStyle = TextStyles.withStyle(currentStyle, styleMask)
  }
  def setColor(foreground: Int, background: Int) {
    applyCurrentStyle
    currentStyle = TextStyles.withColors(currentStyle, foreground, background)
  }
  def setFont(fontnum: Int) {
    applyCurrentStyle
    currentStyle = TextStyles.withFont(currentStyle, fontnum)
  }
  def applyCurrentStyle {
    runBuffer.add(StyledText(currentText.toString, currentStyle))
    currentText = new StringBuilder
  }
  def grabRuns: LinkedList[StyledText] = {
    applyCurrentStyle
    val result = runBuffer
    clear
    result
  }
}
