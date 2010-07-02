/*
 * Created on 2010/04/08
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

import java.util.logging._
import org.zmpp.base._

/**
 * Glk implementation. It is implemented as a facade to the Glk's
 * subsystems, every call is delegated to be handled by the specialized
 * subsystem.
 * In the same manner, the GlkDispatch interface just acts as communication
 * layer to the Glulx VM.
 *
 * The central concept is to implement as much system-independent code in this
 * base Glk as possible. Because of this, the signature of the methods are
 * a little different than in the C implementation. Where a function takes
 * a memory reference, the caller also has to provide a VMState object.
 * VMState provides memory and stack access. I did not want to make Glk
 * have a direct dependency to the virtual machine, so the user has to pass
 * it in case she/he wants to use it directly. The other way is GlkDispatch,
 * which naturally has to contain the VM dependency.
 *
 * There are a couple of interfaces which are UI technology specific and
 * are to be implemented by the user interface:
 *
 * - GlkScreenUI (GlkWindowUI): implement the UI peers to represent windows and
 *   screens
 * - GlkSoundSystem TODO
 */
class Glk(val eventManager: EventManager) {
  val logger = Logger.getLogger("glk")
  private val soundSystem  = new GlkSoundSystem
  private val ioSystem     = new GlkIOSystem
  private val windowSystem = new GlkWindowSystem
  private val fileSystem   = new GlkFileSystem

  windowSystem.reset(ioSystem)
  // WW: we actually do not need these three statements, Glulxe opens a
  // FileStream and MemoryStream
  // for the game file at program start, this is just for debugging to
  // compare the logs to Glulxe
  ioSystem.registerStream(new DummyStream)
  ioSystem.registerStream(new DummyStream)
  fileSystem.createFileRefByName(0, "dummy", 0)  

  def screenUI_=(screenUI: GlkScreenUI) {
    windowSystem.screenUI = screenUI
    eventManager.screenUI = screenUI
  }
  def screenUI = windowSystem.screenUI
  
  def nativeSoundSystem_=(nativeSoundSystem: NativeSoundSystem) {
    soundSystem.nativeSoundSystem = nativeSoundSystem
  }
  def nativeSoundSystem = soundSystem.nativeSoundSystem
  
  // ***********************************************************************
  // ***** glk_* Functions
  // **************************************
  def gestalt(selector: GestaltSelector.Value, arg: Int) : Int = {
    //logger.info("glk_gestalt(%s, %d)".format(selector.toString, arg))
    import GestaltSelector._
    selector match {
      case Version              => 0x00000700
      case CharInput            => 1
      case LineInput            => 1
      case CharOutput           => 1
      case MouseInput           => 1
      case Timer                => 1
      case Graphics             => 1
      case DrawImage            => 1
      case Sound                => 1
      case SoundVolume          => 1
      case SoundNotify          => 1
      case Hyperlinks           => 1
      case HyperlinkInput       => 1
      case SoundMusic           => 1
      case GraphicsTransparency => 1
      case Unicode              => 1
      case _                    =>
        throw new IllegalArgumentException("unknown selector: " + selector.toString)
    }
  }

  def char_to_lower(c: Char): Char = Character.toLowerCase(c)
  def char_to_upper(c: Char): Char = Character.toUpperCase(c)
  def buffer_to_lower_case_uni(state: VMState, buf: Int, len: Int, numchars: Int): Int = {
    for (i <- 0 until numchars) {
      val addr = buf + i * 4
      state.setMemIntAt(addr, Character.toLowerCase(state.memIntAt(addr).toChar))
    }
    numchars
  }
  def buffer_to_upper_case_uni(state: VMState, buf: Int, len: Int, numchars: Int): Int = {
    for (i <- 0 until numchars) {
      val addr = buf + i * 4
      state.setMemIntAt(addr, Character.toUpperCase(state.memIntAt(addr).toChar))
    }
    numchars
  }
  def buffer_to_title_case_uni(state: VMState, buf: Int, len: Int, numchars: Int, lowerrest: Int): Int = {
    state.setMemIntAt(buf, Character.toUpperCase(state.memIntAt(buf).toChar))
    if (lowerrest != 0) {
      for (i <- 1 until numchars) {
        val addr = buf + i * 4
        state.setMemIntAt(addr, Character.toLowerCase(state.memIntAt(addr).toChar))
      }
    }
    numchars
  }

  // Printing
  def put_buffer(state: VMState, buf: Int, len: Int) {
    for (i <- 0 until len) put_char(state.memByteAt(buf + i).toChar)
  }
  def put_buffer_uni(state: VMState, buf: Int, len: Int) {
    for (i <- 0 until len) put_char_uni(state.memIntAt(buf + i * 4))
  }
  def put_char(c: Char) = ioSystem.putChar(c)
  def put_char_uni(c: Int) = ioSystem.putCharUni(c)
  def put_string(state: VMState, s: Int) = {
    var strPtr = s
    var currentChar = state.memByteAt(strPtr)
    while (currentChar != 0) {
      put_char((currentChar & 0xffff).asInstanceOf[Char])
      strPtr += 1
      currentChar = state.memByteAt(strPtr)
    }
  }
  def put_string_uni(state: VMState, s: Int) = {
    var strPtr = s
    var currentChar = state.memIntAt(strPtr)
    while (currentChar != 0) {
      put_char_uni(currentChar)
      strPtr += 4
      currentChar = state.memIntAt(strPtr)
    }
  }
  // Styles
  def stylehint_set(wintype: Int, style: Int, hint: Int, value: Int) {
    windowSystem.setStyleHint(GlkWindowType(wintype), style, hint, value)
  }
  def stylehint_clear(wintype: Int, style: Int, hint: Int) {
    windowSystem.clearStyleHint(GlkWindowType(wintype), style, hint)
  }

  // file references
  def fileref_create_by_name(usage: Int, name: String, rock: Int): Int = {
    fileSystem.createFileRefByName(usage, name, rock)
  }
  def fileref_create_by_prompt(usage: Int, fmode: Int, rock: Int): Int = {
    val file = screenUI.selectFileByDialog(fmode)
    if (file == null) 0
    else fileSystem.createFileRefByFile(usage, fmode, file, rock)
  }
  def fileref_destroy(fileRefId: Int) = fileSystem.destroy(fileRefId)
  def fileref_does_file_exist(fileRefId: Int) = {
    if (fileSystem.doesFileExist(fileRefId)) 1 else 0
  }
  def fileref_get_rock(fileRefId: Int) = fileSystem.getRockForFileRef(fileRefId)
  def fileref_iterate(fref: Int): GlkIterateResult = {
    val fileRef = fileSystem.iterate(fref)
    val fileRefId = if (fileRef == null) 0 else fileRef.id
    val rock = if (fileRef == null) 0 else fileRef.rock
    new GlkIterateResult(fileRefId, rock)
  }

  // Stream functions
  def put_buffer_stream(state: VMState, streamId: Int, buf: Int, len: Int) {
    for (i <- 0 until len) put_char_stream(streamId, state.memByteAt(buf + i).toChar)
  }
  def put_buffer_stream_uni(state: VMState, streamId: Int, buf: Int, len: Int) {
    for (i <- 0 until len) put_char_stream_uni(streamId, state.memIntAt(buf + i * 4))
  }
  def put_char_stream(streamId: Int, c: Char)     = ioSystem.putChar(streamId, c)
  def put_char_stream_uni(streamId: Int, c: Int)  = ioSystem.putCharUni(streamId, c)
  def put_string_stream(state: VMState, streamId: Int, s: Int) = {
    var strPtr = s
    var currentChar = state.memByteAt(strPtr)
    while (currentChar != 0) {
      put_char_stream(streamId, (currentChar & 0xffff).asInstanceOf[Char])
      strPtr += 1
      currentChar = state.memByteAt(strPtr)
    }
  }
  def put_string_stream_uni(state: VMState, streamId: Int, s: Int) = {
    var strPtr = s
    var currentChar = state.memIntAt(strPtr)
    while (currentChar != 0) {
      put_char_stream_uni(streamId, currentChar)
      strPtr += 4
      currentChar = state.memIntAt(strPtr)
    }
  }

  def set_style(value: Int) = ioSystem.currentStyle = value
  def stream_close(streamId: Int): GlkStreamCloseStruct = ioSystem.closeStream(streamId)
  def stream_get_current: Int = ioSystem.currentStreamId
  def stream_get_position(streamId: Int) = ioSystem.getPosition(streamId)
  def stream_get_rock(streamId: Int) = ioSystem.getRock(streamId)
  def stream_iterate(strId: Int):GlkIterateResult = {
    val stream = ioSystem.iterate(strId)
    val streamId = if (stream == null) 0 else stream.id
    val rock     = if (stream == null) 0 else stream.rock
    new GlkIterateResult(streamId, rock)
  }
  def stream_open_file(fileRefId: Int, fmode: Int, rock: Int) = {
    logger.info(
      "glk_stream_open_file, fileRefId = %d, fmode = #$%02x, rock = %d".format(
           fileRefId, fmode, rock))
    val fileStream = fileSystem.openFile(fileRefId, fmode, rock)
    if (fileStream != null) ioSystem.registerStream(fileStream)
    else 0
  }
  def stream_open_memory(state: VMState, buf: Int, buflen: Int, fmode: Int, rock: Int) = {
    ioSystem.registerStream(
      MemoryStreamFactory.createMemoryStream8(state, buf, buflen, fmode, rock))
  }
  def stream_open_memory_uni(state: VMState, buf: Int, buflen: Int, fmode: Int, rock: Int) = {
    ioSystem.registerStream(
      MemoryStreamFactory.createMemoryStream32(state, buf, buflen, fmode, rock))
  }
  def stream_set_current(streamId: Int) {
    ioSystem.currentStreamId = streamId
  }
  def stream_set_position(streamId: Int, pos: Int, seekMode: Int) {
    ioSystem.setPosition(streamId, pos, seekMode)
  }
  
  // Window functions
  def set_window(windowId: Int) {
    ioSystem.currentStream = if (windowId == 0) null
      else windowSystem.outputStreamForWindow(windowId)
  }
  def window_clear(winId: Int) = windowSystem.clearWindow(winId)
  def window_close(winId: Int) = windowSystem.closeWindow(winId)
  def window_get_root          = windowSystem.rootWindowId

  def window_iterate(winId: Int): GlkIterateResult = {
    val window = windowSystem.iterate(winId)
    val windowId = if (window == null) 0 else window.id
    val rock = if (window != null) window.rock else 0
    new GlkIterateResult(windowId, rock)
  }
  def window_get_parent(winId: Int)  = windowSystem.getParent(winId)
  def window_get_rock(winId: Int)    = windowSystem.getRock(winId)
  def window_get_sibling(winId: Int) = windowSystem.getSibling(winId)
  def window_get_size(winId: Int)    = windowSystem.getSize(winId)
  def window_get_stream(winId: Int)  = windowSystem.getStreamId(winId)
  def window_get_type(winId: Int)    = windowSystem.getType(winId)
  def window_move_cursor(winId: Int, xpos: Int, ypos: Int) {
    windowSystem.moveCursor(winId, xpos, ypos)
  }
  def window_open(split: Int, method: Int, size: Int, wintype: Int,
                      rock: Int) = {
    windowSystem.open(split, method, size, wintype, rock)
  }
  def window_set_arrangement(winId: Int, method: Int, size: Int, keywin: Int) {
    //printf("window_set_arrangement win: %d, method: %d, size: %d, keywin: %d\n",
    //  winId, method, size, keywin)
    windowSystem.setArrangement(winId, method, size, keywin)
  }

  // Events
  def cancel_char_event(winId: Int) {
    logger.info("glk_cancel_char_event(%d)".format(winId))
    eventManager.removeInputRequestInWindow(winId, GlkEventType.CharInput)
  }
  def cancel_line_event(winId: Int, eventPtr: Int) {
    logger.info("glk_cancel_line_event(%d, $%02x)".format(winId, eventPtr))
    eventManager.cancelLineEvent(winId, eventPtr)
  }
  def cancel_mouse_event(winId: Int) {
    logger.info("glk_cancel_mouse_event(%d)".format(winId))
    eventManager.removeInputRequestInWindow(winId, GlkEventType.MouseInput)
  }
  def request_char_event(winId: Int) {
    logger.info("glk_request_char_event(%d)".format(winId))
    eventManager.addCharInputRequest(winId)
  }
  def request_hyperlink_event(winId: Int) {
    logger.info("glk_request_hyperlink_event(%d)".format(winId))
    eventManager.addHyperlinkEventRequest(winId)
  }
  def request_line_event(winId: Int, buf: Int, maxlen: Int, initlen: Int) {
    logger.info("glk_request_line_event(%d)".format(winId))
    eventManager.addLineInputRequest(winId, buf, maxlen, initlen)
  }
  def request_mouse_event(winId: Int) {
    logger.info("glk_request_mouse_event(%d)".format(winId))
    eventManager.addMouseInputRequest(winId)
  }
  def request_timer_events(millis: Int) {
    logger.info("glk_request_timer_events(%d ms)".format(millis))
    eventManager.requestTimerEvents(millis)
  }
  def select(eventPtr: Int) = eventManager.select(eventPtr)
  def select_poll(eventPtr: Int) = eventManager.selectPoll(eventPtr)

  // Graphics
  def image_draw(winId: Int, imageNum: Int, val1: Int, val2: Int): Int = {
    windowSystem.drawImage(winId, imageNum, val1, val2)
  }
  def image_draw_scaled(winId: Int, imageNum: Int, val1: Int, val2: Int,
                            width: Int, height: Int): Int = {
    windowSystem.drawScaledImage(winId, imageNum,
                                 val1, val2, width, height)
  }
  def image_get_info(imageNum:Int): GlkDimension = {
    windowSystem.imageSize(imageNum)
  }

  def window_erase_rect(winId: Int, left: Int, top: Int, width: Int, height: Int) {
    windowSystem.eraseRect(winId, left, top, width, height)
  }
  def window_fill_rect(winId: Int, color: Int, left: Int, top: Int,
                           width: Int, height: Int) {
    windowSystem.fillRect(winId, color, left, top, width, height)
  }
  def window_set_background_color(winId: Int, color: Int) {
    windowSystem.setBackgroundColor(winId, color)
  }
  
  // hyperlinks
  def set_hyperlink(linkval: Int) = ioSystem.setHyperlink(linkval)
  
  // sound
  def schannel_create(rock: Int): Int = soundSystem.createChannel(rock)
  def schannel_destroy(channelId: Int) = soundSystem.destroyChannel(channelId)
  def schannel_get_rock(channelId: Int) = soundSystem.getRock(channelId)
  def schannel_iterate(channelId: Int): GlkIterateResult = {
    soundSystem.iterate(channelId)
  }
  def schannel_play(channelId: Int, soundNum: Int): Int = {
    soundSystem.play(channelId, soundNum, 1, 0)
  }
  def schannel_play_ext(channelId: Int, soundNum: Int, repeats: Int, notify: Int): Int = {
    soundSystem.play(channelId, soundNum, repeats, notify)
  }
  def schannel_set_volume(channelId: Int, volume: Int) = soundSystem.setVolume(channelId, volume)
  def schannel_stop(channelId: Int) = soundSystem.stopChannel(channelId)
  def sound_load_hint(soundNum: Int, flag: Int) {
    // ignored
  }
  
  // OS integration
  // note that yield is a Scala keyword, so we need to quote it as a symbol
  def tick = Thread.`yield`

  // ************************************************************************
  // ****** Java-only enhancements (not available in dispatch)
  // ****** Glk is a C-interface, ZMPP/Glk tries to be consistent with
  // ****** C-Glk as much as possible. We provide a couple of Java-style
  // ****** methods here that simplify usage in a JVM environment
  // ************************************************************************
  /*
   * It makes sense to provide a print method for Java strings.
   */
  def put_java_string(s: String) = ioSystem.putJavaString(s)
}


