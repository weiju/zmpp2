/*
 * Created on 2010/06/07
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

class GlkDispatch(_state: VMState, glk: Glk) {
  val dispatchLogger = Logger.getLogger("glk.dispatch")

  def dispatch(id: Int, args: Array[Int]) : Int = {
    val selector = FunctionSelector(id)
/*
    val builder = new StringBuilder
    builder.append("(")
    for (i <- 0 until args.length) {
      if (i > 0) builder.append(", ")
      builder.append(args(i))
    }
    builder.append(")")
    dispatchLogger.info("@@%s%s".format(selector.toString, builder.toString))
*/
    import FunctionSelector._
    selector match {
      case Exit                     => _exit(args)
      case SetInterruptHandler      => _set_interrupt_handler(args)
      case Tick                     => _tick(args)
      case Gestalt                  => _gestalt(args)
      case GestaltExt               => _gestalt_ext(args)
      case WindowIterate            => _window_iterate(args)
      case WindowGetRock            => _window_get_rock(args)
      case WindowGetRoot            => _window_get_root(args)
      case WindowOpen               => _window_open(args)
      case WindowClose              => _window_close(args)
      case WindowGetSize            => _window_get_size(args)
      case WindowSetArrangement     => _window_set_arrangement(args)
      case WindowGetArrangement     => _window_get_arrangement(args)
      case WindowGetType            => _window_get_type(args)
      case WindowGetParent          => _window_get_parent(args)
      case WindowClear              => _window_clear(args)
      case WindowMoveCursor         => _window_move_cursor(args)
      case WindowGetStream          => _window_get_stream(args)
      case WindowSetEchoStream      => _window_set_echo_stream(args)
      case WindowGetEchoStream      => _window_get_echo_stream(args)
      case SetWindow                => _set_window(args)
      case WindowGetSibling         => _window_get_sibling(args)
      case StreamIterate            => _stream_iterate(args)
      case StreamGetRock            => _stream_get_rock(args)
      case StreamOpenFile           => _stream_open_file(args)
      case StreamOpenMemory         => _stream_open_memory(args)
      case StreamClose              => _stream_close(args)
      case StreamSetPosition        => _stream_set_position(args)
      case StreamGetPosition        => _stream_get_position(args)
      case StreamSetCurrent         => _stream_set_current(args)
      case StreamGetCurrent         => _stream_get_current(args)
      case FileRefCreateTemp        => _fileref_create_temp(args)
      case FileRefCreateByName      => _fileref_create_by_name(args)
      case FileRefCreateByPrompt    => _fileref_create_by_prompt(args)
      case FileRefDestroy           => _fileref_destroy(args)
      case FileRefIterate           => _fileref_iterate(args)
      case FileRefGetRock           => _fileref_get_rock(args)
      case FileRefDoesFileExist     => _fileref_does_file_exist(args)
      case FileRefCreateFromFileRef => _fileref_create_from_fileref(args)
      case PutChar                  => _put_char(args)
      case PutCharStream            => _put_char_stream(args)
      case PutString                => _put_string(args)
      case PutStringStream          => _put_string_stream(args)
      case PutBuffer                => _put_buffer(args)
      case PutBufferStream          => _put_buffer_stream(args)
      case SetStyle                 => _set_style(args)
      case SetStyleStream           => _set_style_stream(args)
      case GetCharStream            => _get_char_stream(args)
      case GetLineStream            => _get_line_stream(args)
      case GetBufferStream          => _get_buffer_stream(args)
      case CharToLower              => _char_to_lower(args)
      case CharToUpper              => _char_to_upper(args)
      case StyleHintSet             => _stylehint_set(args)
      case StyleHintClear           => _stylehint_clear(args)
      case StyleDistinguish         => _style_distinguish(args)
      case StyleMeasure             => _style_measure(args)
      case Select                   => _select(args)
      case SelectPoll               => _select_poll(args)
      case RequestLineEvent         => _request_line_event(args)
      case CancelLineEvent          => _cancel_line_event(args)
      case RequestCharEvent         => _request_char_event(args)
      case CancelCharEvent          => _cancel_char_event(args)
      case RequestMouseEvent        => _request_mouse_event(args)
      case CancelMouseEvent         => _cancel_mouse_event(args)
      case RequestTimerEvents       => _request_timer_events(args)
      case ImageGetInfo             => _image_get_info(args)
      case ImageDraw                => _image_draw(args)
      case ImageDrawScaled          => _image_draw_scaled(args)
      case WindowFlowBreak          => _window_flow_break(args)
      case WindowEraseRect          => _window_erase_rect(args)
      case WindowFillRect           => _window_fill_rect(args)
      case WindowSetBackgroundColor => _window_set_background_color(args)
      case SChannelIterate          => _schannel_iterate(args)
      case SChannelGetRock          => _schannel_get_rock(args)
      case SChannelCreate           => _schannel_create(args)
      case SChannelDestroy          => _schannel_destroy(args)
      case SChannelPlay             => _schannel_play(args)
      case SChannelPlayExt          => _schannel_play_ext(args)
      case SChannelStop             => _schannel_stop(args)
      case SChannelSetVolume        => _schannel_set_volume(args)
      case SoundLoadHint            => _sound_load_hint(args)
      case SetHyperlink             => _set_hyperlink(args)
      case SetHyperlinkStream       => _set_hyperlink_stream(args)
      case RequestHyperlinkEvent    => _request_hyperlink_event(args)
      case CancelHyperlinkEvent     => _cancel_hyperlink_event(args)
      case BufferToLowerCaseUni     => _buffer_to_lower_case_uni(args)
      case BufferToUpperCaseUni     => _buffer_to_upper_case_uni(args)
      case BufferToTitleCaseUni     => _buffer_to_title_case_uni(args)
      case PutCharUni               => _put_char_uni(args)
      case PutStringUni             => _put_string_uni(args)      
      case PutBufferUni             => _put_buffer_uni(args)
      case PutCharStreamUni         => _put_char_stream_uni(args)
      case PutStringStreamUni       => _put_string_stream_uni(args)
      case PutBufferStreamUni       => _put_buffer_stream_uni(args)
      case GetCharStreamUni         => _get_char_stream_uni(args)
      case GetBufferStreamUni       => _get_buffer_stream_uni(args)
      case GetLineStreamUni         => _get_line_stream_uni(args)
      case StreamOpenFileUni        => _stream_open_file_uni(args)      
      case StreamOpenMemoryUni      => _stream_open_memory_uni(args)
      case RequestCharEventUni      => _request_char_event_uni(args)
      case RequestLineEventUni      => _request_line_event_uni(args)
      case _ =>
        throw new IllegalArgumentException("unknown GLK id: $%02x".format(id))
    }
  }

  // ***********************************************************************
  // ***** Dispatch Interface
  // ***** These functions just delegate to their respective
  // ***** static glk_* functions. The entry point is the dispatch()
  // ***** method, usually called by the glk instruction in Glulx
  // ***********************************************************************
  private def _buffer_to_lower_case_uni(args: Array[Int]): Int = {
    glk.buffer_to_lower_case_uni(_state, args(0), args(1), args(2))
  }
  private def _buffer_to_upper_case_uni(args: Array[Int]): Int = {
    glk.buffer_to_upper_case_uni(_state, args(0), args(1), args(2))
  }
  private def _buffer_to_title_case_uni(args: Array[Int]): Int = {
    glk.buffer_to_title_case_uni(_state, args(0), args(1), args(2), args(3))
  }

  private def _cancel_char_event(args: Array[Int]): Int = {
    glk.cancel_char_event(args(0))
    0
  }
  private def _cancel_hyperlink_event(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_cancel_hyperlink_event not supported yet")
  }
  private def _cancel_line_event(args: Array[Int]): Int = {
    glk.cancel_line_event(args(0), args(1))
    0
  }
  private def _cancel_mouse_event(args: Array[Int]): Int = {
    glk.cancel_mouse_event(args(0))
    0
  }
  private def _char_to_lower(args: Array[Int]): Int = {
    glk.char_to_lower((args(0) & 0xff).asInstanceOf[Char])
  }
  private def _char_to_upper(args: Array[Int]): Int = {
    glk.char_to_upper((args(0) & 0xff).asInstanceOf[Char])
  }
  private def _exit(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_exit not supported yet")
  }
  private def _fileref_create_by_name(args: Array[Int]): Int = {
    glk.fileref_create_by_name(args(0), cstringAt(args(1)), args(2))
  }
  private def _fileref_create_by_prompt(args: Array[Int]): Int = {
    glk.fileref_create_by_prompt(args(0), args(1), args(2))
  }
  private def _fileref_create_from_fileref(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_fileref_create_from_fileref not supported yet")
  }
  private def _fileref_create_temp(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_fileref_create_temp not supported yet")
  }
  private def _fileref_destroy(args: Array[Int]): Int = {
    glk.fileref_destroy(args(0))
    0
  }
  private def _fileref_does_file_exist(args: Array[Int]): Int = {
    glk.fileref_does_file_exist(args(0))
  }
  private def _fileref_get_rock(args: Array[Int]): Int = {
    glk.fileref_get_rock(args(0))
  }
  private def _fileref_iterate(args: Array[Int]): Int = {
    returnIterateResult(glk.fileref_iterate(args(0)), args(1))
  }
  private def _gestalt(args: Array[Int]): Int = {
    glk.gestalt(GestaltSelector(args(0)), args(1))
  }
  private def _gestalt_ext(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_gestalt_ext not supported yet")
  }
  private def _get_buffer_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_get_buffer_stream not supported yet")
  }
  private def _get_buffer_stream_uni(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_get_buffer_stream_uni not supported yet")
  }
  private def _get_char_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_get_char_stream not supported yet")
  }
  private def _get_char_stream_uni(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_get_char_stream_uni not supported yet")
  }
  private def _get_line_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_get_line_stream not supported yet")
  }
  private def _get_line_stream_uni(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_get_line_stream_uni not supported yet")
  }
  private def _image_draw(args: Array[Int]): Int = {
    glk.image_draw(args(0), args(1), args(2), args(3))
  }
  private def _image_draw_scaled(args: Array[Int]): Int = {
    glk.image_draw_scaled(args(0), args(1), args(2), args(3), args(4), args(5))
  }
  private def _image_get_info(args: Array[Int]): Int = {
    val dim = glk.image_get_info(args(0))
    val widthPtr = args(1)
    val heightPtr = args(2)
    if (dim == null) 0
    else {
      if (widthPtr  != 0) _state.setMemIntAt(widthPtr, dim.width)
      if (heightPtr != 0) _state.setMemIntAt(heightPtr, dim.height)
      1
    }
  }
  private def _put_buffer(args: Array[Int]): Int = {
    glk.put_buffer(_state, args(0), args(1))
    0
  }
  private def _put_buffer_stream(args: Array[Int]): Int = {
    glk.put_buffer_stream(_state, args(0), args(1), args(2))
    0
  }
  private def _put_buffer_stream_uni(args: Array[Int]): Int = {
    glk.put_buffer_stream_uni(_state, args(0), args(1), args(2))
    0
  }
  private def _put_buffer_uni(args: Array[Int]): Int = {
    glk.put_buffer_uni(_state, args(0), args(1))
    0
  }
  private def _put_char(args: Array[Int]): Int = {
    glk.put_char((args(0) & 0xffff).asInstanceOf[Char])
    0
  }
  private def _put_char_uni(args: Array[Int]): Int = {
    glk.put_char_uni(args(0))
    0
  }
  private def _put_char_stream(args: Array[Int]): Int = {
    glk.put_char_stream(args(0), args(1).toChar)
    0
  }
  private def _put_char_stream_uni(args: Array[Int]): Int = {
    glk.put_char_stream_uni(args(0), args(1))
    0
  }
  private def _put_string(args: Array[Int]): Int = {
    glk.put_string(_state, args(0))
    0
  }
  private def _put_string_stream(args: Array[Int]): Int = {
    glk.put_string_stream(_state, args(0), args(1))
    0
  }
  private def _put_string_stream_uni(args: Array[Int]): Int = {
    glk.put_string_stream_uni(_state, args(0), args(1))
    0
  }
  private def _put_string_uni(args: Array[Int]): Int = {
    glk.put_string_uni(_state, args(0))
    0
  }
  private def _request_char_event(args: Array[Int]): Int = {
    glk.request_char_event(args(0))
    0
  }
  private def _request_char_event_uni(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_request_char_event_uni not supported yet")
  }
  private def _request_hyperlink_event(args: Array[Int]): Int = {
    glk.request_hyperlink_event(args(0))
    0
  }
  private def _request_line_event(args: Array[Int]): Int = {
    glk.request_line_event(args(0), args(1), args(2), args(3))
    0
  }
  private def _request_line_event_uni(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_request_line_event_uni not supported yet")
  }
  private def _request_mouse_event(args: Array[Int]): Int = {
    glk.request_mouse_event(args(0))
    0
  }
  private def _request_timer_events(args: Array[Int]): Int = {
    glk.request_timer_events(args(0))
    0
  }
  private def _schannel_create(args: Array[Int]): Int = glk.schannel_create(args(0))
  private def _schannel_destroy(args: Array[Int]): Int = {
    glk.schannel_destroy(args(0))
    0
  }
  private def _schannel_get_rock(args: Array[Int]): Int = glk.schannel_get_rock(args(0))
  private def _schannel_iterate(args: Array[Int]): Int = {
    returnIterateResult(glk.schannel_iterate(args(0)), args(1))
  }
  private def _schannel_play(args: Array[Int]): Int = glk.schannel_play(args(0), args(1))
  private def _schannel_play_ext(args: Array[Int]): Int = {
    glk.schannel_play_ext(args(0), args(1), args(2), args(3))
  }
  private def _schannel_set_volume(args: Array[Int]): Int = {
    glk.schannel_set_volume(args(0), args(1))
    0
  }
  private def _schannel_stop(args: Array[Int]): Int = {
    glk.schannel_stop(args(0))
    0
  }
  private def _select(args: Array[Int]): Int = {
    glk.select(args(0))
    0
  }
  private def _select_poll(args: Array[Int]): Int = {
    glk.select_poll(args(0))
    0
  }
  private def _set_hyperlink(args: Array[Int]): Int = {
    glk.set_hyperlink(args(0))
    0
  }
  private def _set_hyperlink_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_set_hyperlink_stream not supported yet")
  }
  private def _set_interrupt_handler(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_set_interrupt_handler not supported yet")
  }
  private def _set_style(args: Array[Int]): Int = {
    glk.set_style(args(0))
    0
  }
  private def _set_style_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_set_style_stream not supported yet")
  }
  private def _set_window(args: Array[Int]): Int = {
    glk.set_window(args(0))
    0
  }
  private def _sound_load_hint(args: Array[Int]): Int = {
    glk.sound_load_hint(args(0), args(1))
    0
  }
  private def _stream_close(args: Array[Int]): Int = {
    val result = glk.stream_close(args(0))
    val resultRef = args(1)
    if (resultRef == -1) {
      // push on stack
      _state.pushInt(result.readCount)
      _state.pushInt(result.writeCount)
    } else if (resultRef != 0) {
      // write to mem
      _state.setMemIntAt(resultRef, result.readCount)
      _state.setMemIntAt(resultRef + 4, result.writeCount)
    }
    0
  }
  private def _stream_get_current(args: Array[Int]) = glk.stream_get_current
  private def _stream_get_position(args: Array[Int]) = glk.stream_get_position(args(0))
  private def _stream_get_rock(args: Array[Int]) = glk.stream_get_rock(args(0))
  private def _stream_iterate(args: Array[Int]): Int = {
    returnIterateResult(glk.stream_iterate(args(0)), args(1))
  }
  private def _stream_open_file(args: Array[Int]) = {
    glk.stream_open_file(args(0), args(1), args(2))
  }
  private def _stream_open_file_uni(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_stream_open_file_uni not supported yet")
  }
  private def _stream_open_memory(args: Array[Int]) = {
    glk.stream_open_memory(_state, args(0), args(1), args(2), args(3))
  }
  private def _stream_open_memory_uni(args: Array[Int]) = {
    glk.stream_open_memory_uni(_state, args(0), args(1), args(2), args(3))
  }
  private def _stream_set_current(args: Array[Int]): Int = {
    glk.stream_set_current(args(0))
    0
  }
  private def _stream_set_position(args: Array[Int]): Int = {
    glk.stream_set_position(args(0), args(1), args(2))
    0
  }
  private def _style_distinguish(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_style_distinguish not supported yet")
  }
  private def _style_measure(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_style_measure not supported yet")
  }
  private def _stylehint_clear(args: Array[Int]): Int = {
    glk.stylehint_clear(args(0), args(1), args(2))
    0
  }
  private def _stylehint_set(args: Array[Int]): Int = {
    glk.stylehint_set(args(0), args(1), args(2), args(3))
    0
  }
  private def _tick(args: Array[Int]): Int = {
    glk.tick
    0
  }
  private def _window_clear(args: Array[Int]): Int = {
    glk.window_clear(args(0))
    0
  }
  private def _window_close(args: Array[Int]): Int = {
    val charsWritten = glk.window_close(args(0))
    val streamResultPtr = args(1)
    if (streamResultPtr != 0) _state.setMemIntAt(streamResultPtr, charsWritten)
    0
  }
  private def _window_erase_rect(args: Array[Int]): Int = {
    glk.window_erase_rect(args(0), args(1), args(2), args(3), args(4))
    0
  }
  private def _window_fill_rect(args: Array[Int]): Int = {
    glk.window_fill_rect(args(0), args(1), args(2), args(3), args(4), args(5))
    0
  }
  private def _window_flow_break(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_window_flow_break not supported yet")
  }
  private def _window_get_arrangement(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_window_get_arrangement not supported yet")
  }
  private def _window_get_parent(args: Array[Int])  = glk.window_get_parent(args(0))
  private def _window_get_rock(args: Array[Int])    = glk.window_get_rock(args(0))
  private def _window_get_root(args: Array[Int])    = glk.window_get_root
  private def _window_get_sibling(args: Array[Int]) = glk.window_get_sibling(args(0))
  private def _window_get_size(args: Array[Int]): Int = {
    val dim = glk.window_get_size(args(0))
    val widthPtr  = args(1)
    val heightPtr = args(2)

    if (widthPtr == -1) _state.pushInt(dim.width)
    else if (widthPtr != 0) _state.setMemIntAt(widthPtr, dim.width)
    if (heightPtr == -1) _state.pushInt(dim.height)
    else if (heightPtr != 0) _state.setMemIntAt(heightPtr, dim.height)
    0
  }
  private def _window_get_stream(args: Array[Int]): Int = glk.window_get_stream(args(0))
  private def _window_get_type(args: Array[Int]): Int = glk.window_get_type(args(0))
  private def _window_iterate(args: Array[Int]): Int = {
    returnIterateResult(glk.window_iterate(args(0)), args(1))
  }
  private def _window_move_cursor(args: Array[Int]): Int = {
    glk.window_move_cursor(args(0), args(1), args(2))
    0
  }
  private def _window_open(args: Array[Int]): Int = {
    glk.window_open(args(0), args(1), args(2), args(3), args(4))
  }
  private def _window_set_arrangement(args: Array[Int]): Int = {
    glk.window_set_arrangement(args(0), args(1), args(2), args(3))
    0
  }
  private def _window_set_background_color(args: Array[Int]): Int = {
    glk.window_set_background_color(args(0), args(1))
    0
  }
  private def _window_set_echo_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_window_set_echo_stream not supported yet")
  }
  private def _window_get_echo_stream(args: Array[Int]): Int = {
    throw new UnsupportedOperationException("@@glk_window_get_echo_stream not supported yet")
  }

  // ***********************************************************************
  // ***** Utility Functions
  // **************************************
  private def returnIterateResult(result: GlkIterateResult, rockPtr: Int) = {
    // Note: -1 for a reference means "stack"
    if (rockPtr == -1) _state.pushInt(result.rock)
    else if (rockPtr != 0) _state.setMemIntAt(rockPtr, result.rock)
    result.id    
  }

  private def cstringAt(address: Int)= {
    val builder = new StringBuilder
    var c = _state.memByteAt(address)
    if (c != 0xe0) {
      throw new IllegalArgumentException("only uncompressed C Strings allowed in Glk")
    }
    var offset = 1
    while (c != 0) {
      c = _state.memByteAt(address + offset)
      builder.append(c.toChar)
      offset += 1
    }
    //printf("CString at $%02x = '%s'\n", address, builder.toString)
    builder.toString
  }
}

