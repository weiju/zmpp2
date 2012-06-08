/*
 * Created on 2010/08/07
 * Copyright (c) 2010-2012, Wei-ju Wu.
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
package org.zmpp.zcode.swing

import java.awt.Color
import org.zmpp.zcode.Colors

// Define _two_ color tables, one for background colors
// and one for foreground tables. The reason is that some
// games (e.g. Varicella) don't really deal with colors properly.
// They rely on the foreground color being brighter than the foreground
// color. Unfortunately, Varicella also assumes that the default foreground
// color is not black.
object BackgroundColors {
  val colorTable = new Array[Color](13)
  // current is never accessed and default is set by screen model
  colorTable(Colors.Black)   = Color.BLACK
  colorTable(Colors.Red)     = new Color(200, 0, 0)
  colorTable(Colors.Green)   = new Color(0, 200, 0)
  colorTable(Colors.Yellow)  = new Color(200, 200, 0)
  colorTable(Colors.Blue)    = new Color(0, 0, 200)
  colorTable(Colors.Magenta) = new Color(200, 0, 200)
  colorTable(Colors.Cyan)    = new Color(0, 200, 200)
  colorTable(Colors.White)   = new Color(255, 255, 255)

  def apply(colornum: Int): Color = {
    colorTable(colornum)
  }
  def setDefault(colornum: Int) {
    colorTable(Colors.Default) = colorTable(colornum)
  }
}
