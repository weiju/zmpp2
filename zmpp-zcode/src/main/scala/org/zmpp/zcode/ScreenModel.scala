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

object Fonts {
  val Normal             = 1
  val Picture            = 2
  val CharacterGfx       = 3
  val Fixed              = 4
}

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

object TextStyles {
  val Roman              = 0
  val ReverseVideo       = 1
  val Bold               = 2
  val Italic             = 4
  val FixedPitch         = 8

  def isRoman(style: Int) = style == Roman
  def isReverseVideo(style: Int) = (style & ReverseVideo) == ReverseVideo
  def isBold(style: Int) = (style & Bold)  == Bold
  def isItalic(style: Int) = (style & Italic) == Italic
  def isFixed(style: Int) = (style & FixedPitch) == FixedPitch

  val DefaultNormal = TextStyle(0, Fonts.Normal, Colors.Default, Colors.Default)
  val DefaultFixed = TextStyle(0, Fonts.Fixed,
                                Colors.Default, Colors.Default)
  val DefaultFixedBlank = StyledChar(' ', DefaultFixed)
}

/*
 * Auxiliary structures to help building screen models.
 */
case class TextStyle(style: Int, fontnum: Int,
                     foreground: Int, background: Int) {
  import TextStyles._
  def isRoman = style == Roman
  def isReverseVideo = (style & ReverseVideo) == ReverseVideo
  def isBold = (style & Bold)  == Bold
  def isItalic = (style & Italic) == Italic
  def isFixed = fontnum == FixedPitch

  def withStyle(newStyle: Int) = {
    TextStyle(newStyle,
              if (TextStyles.isFixed(newStyle)) Fonts.Fixed else fontnum,
              foreground, background)
  }
  def withColor(newForeground: Int, newBackground: Int) = {
    TextStyle(style, fontnum, newForeground, newBackground)
  }
  def withFont(newFontnum: Int) = {
    TextStyle(style, newFontnum, foreground, background)
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
  def flush: Unit
  def putChar(c: Char): Unit
  def clear: Unit
  def cursorPosition_=(pos: (Int, Int)): Unit
  def cursorPosition: (Int, Int)
}

case class StyledText(text: String, style: TextStyle) {
  def isItalic = style.isItalic
  def isBold = style.isBold
  def isReverseVideo = style.isReverseVideo
  def isFixed = style.isFixed
  def foreground = style.foreground
  def background = style.background
}
case class StyledChar(c: Char, style: TextStyle) {
  def isItalic = style.isItalic
  def isBold = style.isBold
  def isReverseVideo = style.isReverseVideo
  def isFixed = style.isFixed
  def foreground = style.foreground
  def background = style.background
}

/*
 * While text grids are not technically "buffered" according to the
 * specification, storing them before flushing them to the screen
 * comes with a lot of advantages:
 * - Game responsiveness appears to be much better when the top window is buffered
 * - Output can be easily clipped
 * - we can store and serialize the state
 */
class TextGridBuffer(numRows: Int, numColumns: Int) {
  import TextStyles._

  private val grid = Array.ofDim[StyledChar](numRows, numColumns)
  fillGridWith(DefaultFixedBlank, 0)

  def fillGridWith(styledChar: StyledChar, startRow: Int=0) {
    printf("fillGridWith, c = '%c', startRow = %d\n", styledChar.c, startRow)
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

  def putChar(c: StyledChar, row: Int, column: Int) {
    if (row >= 0 && row < numRows && column >= 0 && column < numColumns) {
      grid(row)(column) = c
    }
  }
  def charAt(row: Int, column: Int) = grid(row)(column)
}

/**
 * A more sophisticated buffer to store text runs before they are
 * flushed. This is to increase the user response time by letting
 * the game thread run without waiting for the UI thread each time
 * a style change occurs.
 */
class TextRunBuffer {
  import TextStyles._
  private[this] var currentStyle: TextStyle = DefaultNormal
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
  def setStyle(style: Int) {
    applyCurrentStyle
    currentStyle = currentStyle.withStyle(style)
  }
  def setColor(foreground: Int, background: Int) {
    applyCurrentStyle
    currentStyle = currentStyle.withColor(foreground, background)
  }
  def setFont(fontnum: Int) {
    applyCurrentStyle
    currentStyle = currentStyle.withFont(fontnum)
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
