/*
 * Created on 2010/06/07
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

import scala.annotation.switch
import java.util.logging._
import org.zmpp.base._

class GlkDispatch(_state: VMState, glk: Glk) {
  val dispatchLogger = Logger.getLogger("glk.dispatch")

  def dispatch(id: Int, args: Array[Int]) : Int = {
    import FunctionSelector._
/*
    val selector = FunctionSelector(id)
    val builder = new StringBuilder
    builder.append("(")
    for (i <- 0 until args.length) {
      if (i > 0) builder.append(", ")
      builder.append(args(i))
    }
    builder.append(")")
    dispatchLogger.info("@@%s%s".format(selector.toString, builder.toString))
*/
    (id: @switch) match {
      case 0x01  => _exit(args)                   // glk_exit
      case 0x02  => _set_interrupt_handler(args)  // glk_set_interrupt_handler
      case 0x03  => _tick(args)                   // glk_tick
      case 0x04  => _gestalt(args)                // glk_gestalt
      case 0x05  => _gestalt_ext(args)            // glk_gestalt_ext
      case 0x20  => _window_iterate(args)         // glk_window_iterate
      case 0x21  => _window_get_rock(args)        // glk_window_get_rock
      case 0x22  => _window_get_root(args)        // glk_window_get_root
      case 0x23  => _window_open(args)            // glk_window_open
      case 0x24  => _window_close(args)           // glk_window_close
      case 0x25  => _window_get_size(args)        // glk_window_get_size
      case 0x26  => _window_set_arrangement(args) // glk_window_set_arrangement
      case 0x27  => _window_get_arrangement(args) // glk_window_get_arranngement
      case 0x28  => _window_get_type(args)        // glk_window_get_type
      case 0x29  => _window_get_parent(args)      // glk_window_get_parent
      case 0x2a  => _window_clear(args)           // glk_window_clear
      case 0x2b  => _window_move_cursor(args)     // glk_window_move_cursor
      case 0x2c  => _window_get_stream(args)      // glk_window_get_stream
      case 0x2d  => _window_set_echo_stream(args) // glk_window_set_echo_stream
      case 0x2e  => _window_get_echo_stream(args) // glk_window_get_echo_stream
      case 0x2f  => _set_window(args)             // glk_set_window
      case 0x30  => _window_get_sibling(args)     // glk_window_get_sibling
      case 0x40  => _stream_iterate(args)         // glk_stream_iterate
      case 0x41  => _stream_get_rock(args)        // glk_stream_get_rock
      case 0x42  => _stream_open_file(args)       // glk_stream_open_file
      case 0x43  => _stream_open_memory(args)     // glk_stream_open_memory
      case 0x44  => _stream_close(args)           // glk_stream_close
      case 0x45  => _stream_set_position(args)    // glk_stream_set_position
      case 0x46  => _stream_get_position(args)    // glk_stream_get_position
      case 0x47  => _stream_set_current(args)     // glk_stream_set_current
      case 0x48  => _stream_get_current(args)     // glk_stream_get_current
      case 0x60  => _fileref_create_temp(args)    // glk_fileref_create_temp
      case 0x61  => _fileref_create_by_name(args) // glk_fileref_create_by_name
      case 0x62  => _fileref_create_by_prompt(args) // glk_fileref_create_by_prompt
      case 0x63  => _fileref_destroy(args)        // glk_fileref_destroy
      case 0x64  => _fileref_iterate(args)        // glk_fileref_iterate
      case 0x65  => _fileref_get_rock(args)       // glk_fileref_get_rock
      case 0x66  => _fileref_delete_file(args)    // glk_fileref_delete_file
      case 0x67  => _fileref_does_file_exist(args) // glk_fileref_does_file_exist
      case 0x68  => _fileref_create_from_fileref(args) // glk_fileref_create_from_fileref
      case 0x80  => _put_char(args)               // glk_put_char
      case 0x81  => _put_char_stream(args)        // glk_put_char_stream
      case 0x82  => _put_string(args)             // glk_put_string
      case 0x83  => _put_string_stream(args)      // glk_put_string_stream
      case 0x84  => _put_buffer(args)             // glk_put_buffer
      case 0x85  => _put_buffer_stream(args)      // glk_put_buffer_stream
      case 0x86  => _set_style(args)              // glk_set_style
      case 0x87  => _set_style_stream(args)       // glk_set_style_stream
      case 0x90  => _get_char_stream(args)        // glk_get_char_stream
      case 0x91  => _get_line_stream(args)        // glk_get_line_stream
      case 0x92  => _get_buffer_stream(args)      // glk_get_buffer_stream
      case 0xa0  => _char_to_lower(args)          // glk_char_to_lower
      case 0xa1  => _char_to_upper(args)          // glk_char_to_upper
      case 0xb0  => _stylehint_set(args)          // glk_stylehint_set
      case 0xb1  => _stylehint_clear(args)        // glk_stylehint_clear
      case 0xb2  => _style_distinguish(args)      // glk_style_distinguish
      case 0xb3  => _style_measure(args)          // glk_style_measure
      case 0xc0  => _select(args)                 // glk_select
      case 0xc1  => _select_poll(args)            // glk_select_poll
      case 0xd0  => _request_line_event(args)     // glk_request_line_event
      case 0xd1  => _cancel_line_event(args)      // glk_cancel_line_event
      case 0xd2  => _request_char_event(args)     // glk_request_char_event
      case 0xd3  => _cancel_char_event(args)      // glk_cancel_char_event
      case 0xd4  => _request_mouse_event(args)    // glk_request_mouse_event
      case 0xd5  => _cancel_mouse_event(args)     // glk_cancel_mouse_event
      case 0xd6  => _request_timer_events(args)   // glk_request_timer_events
      case 0xe0  => _image_get_info(args)         // glk_image_get_info
      case 0xe1  => _image_draw(args)             // glk_image_draw
      case 0xe2  => _image_draw_scaled(args)      // glk_image_draw_scaled
      case 0xe8  => _window_flow_break(args)      // glk_window_flow_break
      case 0xe9  => _window_erase_rect(args)      // glk_window_erase_rect
      case 0xea  => _window_fill_rect(args)       // glk_window_fill_rect
      case 0xeb  => _window_set_background_color(args) // glk_window_set_background_color
      case 0xf0  => _schannel_iterate(args)       // glk_schannel_iterate
      case 0xf1  => _schannel_get_rock(args)      // glk_schannel_get_rock
      case 0xf2  => _schannel_create(args)        // glk_schannel_create
      case 0xf3  => _schannel_destroy(args)       // glk_destroy
      case 0xf8  => _schannel_play(args)          // glk_schannel_play
      case 0xf9  => _schannel_play_ext(args)      // glk_schannel_play_ext
      case 0xfa  => _schannel_stop(args)          // glk_schannel_stop
      case 0xfb  => _schannel_set_volume(args)    // glk_schannel_set_volume
      case 0xfc  => _sound_load_hint(args)        // glk_sound_load_hint
      case 0x100 => _set_hyperlink(args)          // glk_set_hyperlink
      case 0x101 => _set_hyperlink_stream(args)   // glk_set_hyperlink_stream
      case 0x102 => _request_hyperlink_event(args) // glk_request_hyperlink_event
      case 0x103 => _cancel_hyperlink_event(args) // glk_cancel_hyperlink_event
      case 0x120 => _buffer_to_lower_case_uni(args) // glk_buffer_to_lower_case_uni
      case 0x121 => _buffer_to_upper_case_uni(args) // glk_buffer_to_upper_case_uni
      case 0x122 => _buffer_to_title_case_uni(args) // glk_buffer_to_title_case_uni
      case 0x128 => _put_char_uni(args)           // glk_put_char_uni
      case 0x129 => _put_string_uni(args)         // glk_put_string_uni
      case 0x12a => _put_buffer_uni(args)         // glk_put_buffer_uni
      case 0x12b => _put_char_stream_uni(args)    // glk_put_char_stream_uni
      case 0x12c => _put_string_stream_uni(args)  // glk_put_string_stream_uni
      case 0x12d => _put_buffer_stream_uni(args)  // glk_put_buffer_stream_uni
      case 0x130 => _get_char_stream_uni(args)    // glk_get_char_stream_uni
      case 0x131 => _get_buffer_stream_uni(args)  // glk_get_buffer_stream_uni
      case 0x132 => _get_line_stream_uni(args)    // glk_get_line_stream_uni
      case 0x138 => _stream_open_file_uni(args)   // glk_stream_open_file_uni
      case 0x139 => _stream_open_memory_uni(args) // glk_stream_open_memory_uni
      case 0x140 => _request_char_event_uni(args) // glk_request_char_event_uni
      case 0x141 => _request_line_event_uni(args) // glk_request_line_event_uni
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
    glk.cancel_hyperlink_event(args(0))
    0
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
    glk.exit(_state)
    0
  }
  private def _fileref_create_by_name(args: Array[Int]): Int = {
    glk.fileref_create_by_name(args(0), cstringAt(args(1)), args(2))
  }
  private def _fileref_create_by_prompt(args: Array[Int]): Int = {
    glk.fileref_create_by_prompt(args(0), args(1), args(2))
  }
  private def _fileref_create_from_fileref(args: Array[Int]): Int = {
    glk.fileref_create_from_fileref(args(0), args(1), args(2))
  }
  private def _fileref_create_temp(args: Array[Int]): Int = {
    glk.fileref_create_temp(args(0), args(1))
  }
  private def _fileref_delete_file(args: Array[Int]): Int = {
    glk.fileref_delete_file(args(0))
    0
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
    glk.gestalt(args(0), args(1))
  }
  private def _gestalt_ext(args: Array[Int]): Int = {
    glk.gestalt_ext(_state, args(0), args(1), args(2), args(3))
  }
  private def _get_buffer_stream(args: Array[Int]): Int = {
    glk.get_buffer_stream(_state, args(0), args(1), args(2))
  }
  private def _get_buffer_stream_uni(args: Array[Int]): Int = {
    glk.get_buffer_stream_uni(_state, args(0), args(1), args(2))
  }
  private def _get_char_stream(args: Array[Int]): Int = {
    glk.get_char_stream(args(0))
  }
  private def _get_char_stream_uni(args: Array[Int]): Int = {
    glk.get_char_stream_uni(args(0))
  }
  private def _get_line_stream(args: Array[Int]): Int = {
    glk.get_line_stream(_state, args(0), args(1), args(2))
  }
  private def _get_line_stream_uni(args: Array[Int]): Int = {
    glk.get_line_stream_uni(_state, args(0), args(1), args(2))
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
    glk.request_char_event_uni(args(0))
    0
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
    glk.request_line_event_uni(args(0), args(1), args(2), args(3))
    0
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
    glk.set_hyperlink_stream(args(0), args(1))
    0
  }
  private def _set_interrupt_handler(args: Array[Int]): Int = {
    glk.set_interrupt_handler(_state, args(0))
    0
  }
  private def _set_style(args: Array[Int]): Int = {
    glk.set_style(args(0))
    0
  }
  private def _set_style_stream(args: Array[Int]): Int = {
    glk.set_style_stream(args(0), args(1))
    0
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
    glk.stream_open_file(args(0), args(1), args(2))
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
    glk.style_distinguish(args(0), args(1), args(2))
  }
  private def _style_measure(args: Array[Int]): Int = {
    glk.style_measure(_state, args(0), args(1), args(2), args(3))
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
    glk.window_flow_break(args(0))
    0
  }
  private def _window_get_arrangement(args: Array[Int]): Int = {
    glk.window_get_arrangement(_state, args(0), args(1), args(2), args(3))
    0
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
    glk.window_set_echo_stream(args(0), args(1))
    0
  }
  private def _window_get_echo_stream(args: Array[Int]): Int = {
    glk.window_get_echo_stream(args(0))
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
      throw new IllegalArgumentException(
        "only uncompressed C Strings allowed in Glk")
    }
    var offset = 1
    while (c != 0) {
      c = _state.memByteAt(address + offset)
      builder.append(c.toChar)
      offset += 1
    }
    builder.toString
  }
}

