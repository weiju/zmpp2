package org.zmpp.zcode

import javax.swing._
import java.awt._

/*
 * Implementation of the V6 screen model using Swing components.
 * The V6 screen model is pixel-based, so rendering is entirely done
 * in a custom way, making this much more difficult and more restrictive.
 */
class SwingScreenModelV6 extends JComponent
with OutputStream with InputStream with SwingScreenModel {
  var vm: Machine = null
  private var selected  = true
  val fixedFont         = new Font("Courier New", Font.PLAIN, 14)
  setPreferredSize(new Dimension(640, 480))

  // OutputStream
  def isSelected = selected
  def select(flag: Boolean) = selected = flag
  def putChar(c: Char) { }
  def flush { }

  // InputStream
  def readLine: Int = {
    println("@read_line (TODO)")
    0
  }

  // ScreenModel
  def initUI {
    // now the top window "knows" how large the screen is, so we can set
    // the dimensions and font sizes to the VM
    val g = getGraphics
    val fm = g.getFontMetrics(fixedFont)
    vm.setFontSizeInUnits(fm.charWidth('0'), fm.getHeight)
    vm.setScreenSizeInUnits(getWidth, getHeight)
  }
  def connect(aVm: Machine) {
    vm = aVm
  }
  def setColour(foreground: Int, background: Int, window: Int) {
    printf("@set_colour %d %d %d (TODO)\n", foreground, background, window)
  }
  def setFont(font: Int): Int = {
    printf("@set_font %d (TODO)\n", font)
    1
  }
  def eraseLine(value: Int) {
    printf("@erase_line %d not implemented yet (TODO)\n", value)
  }
  def setTextStyle(aStyle: Int) {
    printf("@set_text_style %d (TODO)\n", aStyle)
  }
  def eraseWindow(windowId: Int) {
    printf("@erase_window %d (TODO)\n", windowId)
  }
  def setWindow(windowId: Int) {
    printf("@set_window %d (TODO)\n", windowId)
  }
  def splitWindow(lines: Int) {
    printf("@split_window %d (TODO)\n", lines)
  }
  def setCursor(line: Int, column: Int) {
    printf("@set_cursor %d %d (TODO)\n", line, column)
  }
  def updateStatusLine { }
  def screenOutputStream = this
  def keyboardStream     = this
  def bufferMode(flag: Int) {
    printf("@buffer_mode %d (TODO)\n", flag)
  }

  // SwingStreamModel
  def readChar { }
}
