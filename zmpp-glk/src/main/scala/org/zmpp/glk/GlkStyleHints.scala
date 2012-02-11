/*
 * Created on 2010/05/09
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
package org.zmpp.glk

/**
 * Styles, style hints and their defaults. I put that in its separate file,
 * because it's long.
 */
object StyleType {
  val Normal       = 0
  val Emphasized   = 1
  val Preformatted = 2
  val Header       = 3
  val Subheader    = 4
  val Alert        = 5
  val Note         = 6
  val BlockQuote   = 7
  val Input        = 8
  val User1        = 9
  val User2        = 10
  val Num          = 11
  def isSupported(id: Int) = id <= 10
}

object StyleHintType {
  val Indentation     = 0
  val ParaIndentation = 1
  val Justification   = 2
  val Size            = 3
  val Weight          = 4
  val Oblique         = 5
  val Proportional    = 6
  val TextColor       = 7
  val BackColor       = 8
  val ReverseColor    = 9
  val Num             = 10
  def isSupported(id: Int) = id <= 9
}

object StyleHintJustification {
  val LeftFlush  = 0
  val LeftRight  = 1
  val Centered   = 2
  val RightFlush = 3
}

object StyleHints {
  import StyleType._
  import StyleHintType._
  import StyleHintJustification._

  val Defaults = Array(
    Array(0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0), // Normal
    Array(0, 0, LeftFlush, 0, 1, 0, 1, -1, -1, 0), // Emphasized
    Array(0, 0, LeftFlush, 0, 0, 0, 0, -1, -1, 0), // Preformatted
    Array(0, 0, LeftFlush, 1, 1, 0, 1, -1, -1, 0), // Header
    Array(0, 0, LeftFlush, 0, 1, 1, 1, -1, -1, 0), // Subheader
    Array(0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0), // Alert
    Array(0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0), // Note
    Array(0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0), // BlockQuote
    Array(0, 0, LeftFlush, 0, 1, 0, 1, -1, -1, 0), // Input
    Array(0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0), // User1
    Array(0, 0, LeftFlush, 0, 0, 0, 1, -1, -1, 0)  // User2
  )
}

class StyleHints {
  private val _hints = Array.ofDim[Int](StyleType.Num,
                                        StyleHintType.Num)
  reset

  def reset {
    var styleNum = 0
    var hintNum = 0
    while (styleNum < StyleType.Num) {
      hintNum = 0
      while (hintNum < StyleHintType.Num) {
        reset(styleNum, hintNum)
        hintNum += 1
      }
      styleNum += 1
    }
  }
  def get(styleNum: Int, hintNum: Int): Int = {
    if (styleNum >= 0 && styleNum < StyleType.Num &&
        hintNum >= 0 && hintNum < StyleHintType.Num) {
      _hints(styleNum)(hintNum)
    } else -1
  }
  def set(styleNum: Int, hintNum: Int, value: Int) {
    if (styleNum >= 0 && styleNum < StyleType.Num &&
        hintNum >= 0 && hintNum < StyleHintType.Num) {
      _hints(styleNum)(hintNum) = value
    }
  }
  def reset(styleNum: Int, hintNum: Int) {
    _hints(styleNum)(hintNum) = StyleHints.Defaults(styleNum)(hintNum)
  }

  def distinguishable(style1: Int, style2: Int): Boolean = {
    if (style1 == style2) false
    else if (style1 < 0 || style2 < 0 ||
             style1 >= StyleType.Num || style2 >= StyleType.Num) false
    else {
      var hintNum = 0
      while (hintNum < StyleHintType.Num) {
        if (_hints(style1)(hintNum) != _hints(style2)(hintNum)) return true
        hintNum += 1
      }
      false
    }
  }
}
