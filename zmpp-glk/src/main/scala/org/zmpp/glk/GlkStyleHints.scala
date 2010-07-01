/*
 * Created on 2010/05/09
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
package org.zmpp.glk

/**
 * Styles, style hints and their defaults. I put that in its separate file,
 * because it's long.
 */
object StyleType extends Enumeration {
  val Normal       = Value("Normal")
  val Emphasized   = Value("Emphasized")
  val Preformatted = Value("Preformatted")
  val Header       = Value("Header")
  val Subheader    = Value("Subheader")
  val Alert        = Value("Alert")
  val Note         = Value("Note")
  val BlockQuote   = Value("BlockQuote")
  val Input        = Value("Input")
  val User1        = Value("User1")
  val User2        = Value("User2")
  def Num          = maxId
  def isSupported(id: Int) = id <= maxId
}

object StyleHintType extends Enumeration {
  val Indentation     = Value("Indentation")
  val ParaIndentation = Value("ParaIndentation")
  val Justification   = Value("Justification")
  val Size            = Value("Size")
  val Weight          = Value("Weight")
  val Oblique         = Value("Oblique")
  val Proportional    = Value("Proportional")
  val TextColor       = Value("TextColor")
  val BackColor       = Value("BackColor")
  val ReverseColor    = Value("ReverseColor")
  val Num             = maxId
  def isSupported(id: Int) = id <= maxId
}

object StyleHintJustification extends Enumeration {
  val LeftFlush  = Value("LeftFlush")
  val LeftRight  = Value("LeftRight")
  val Centered   = Value("Centered")
  val RightFlush = Value("RightFlush")
}

object StyleHints {
  import StyleType._
  import StyleHintType._
  val Defaults = Map(
    Normal -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Emphasized -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 1,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Preformatted -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 0,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Header -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 1,
      Weight          -> 1,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Subheader -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 1,
      Oblique         -> 1,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Alert -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Note -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    BlockQuote -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    Input -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 1,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    User1 -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      ),
    User2 -> Map(
      Indentation     -> 0,
      ParaIndentation -> 0,
      Justification   -> StyleHintJustification.LeftFlush.id,
      Size            -> 0,
      Weight          -> 0,
      Oblique         -> 0,
      Proportional    -> 1,
      TextColor       -> -1,
      BackColor       -> -1,
      ReverseColor    -> 0
      )
  )
}

class StyleHints {
  private val _hints = Array.ofDim[Int](StyleType.Num, StyleHintType.Num)
  reset

  def reset {
    for (styleNum <- 0 until StyleType.Num) {
      for (hintNum <- 0 until StyleHintType.Num) {
        reset(styleNum, hintNum)
      }
    }
  }
  def get(styleNum: Int, hintNum: Int): Int = _hints(styleNum)(hintNum)
  def set(styleNum: Int, hintNum: Int, value: Int) {
    _hints(styleNum)(hintNum) = value
  }
  def reset(styleNum: Int, hintNum: Int) {
    _hints(styleNum)(hintNum) = StyleHints.Defaults(StyleType(styleNum))(StyleHintType(hintNum))
  }  
}


