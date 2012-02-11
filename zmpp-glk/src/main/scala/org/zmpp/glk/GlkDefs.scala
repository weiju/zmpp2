/*
 * Created on 2010/04/08
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

object FunctionSelector extends Enumeration {
  val Exit                     = Value(0x01,  "glk_exit")
  val SetInterruptHandler      = Value(0x02,  "glk_set_interrupt_handler")
  val Tick                     = Value(0x03,  "glk_tick")
  val Gestalt                  = Value(0x04,  "glk_gestalt")
  val GestaltExt               = Value(0x05,  "glk_gestalt_ext")
  val WindowIterate            = Value(0x20,  "glk_window_iterate")
  val WindowGetRock            = Value(0x21,  "glk_window_get_rock")
  val WindowGetRoot            = Value(0x22,  "glk_window_get_root")
  val WindowOpen               = Value(0x23,  "glk_window_open")
  val WindowClose              = Value(0x24,  "glk_window_close")
  val WindowGetSize            = Value(0x25,  "glk_window_get_size")
  val WindowSetArrangement     = Value(0x26,  "glk_window_set_arrangement")
  val WindowGetArrangement     = Value(0x27,  "glk_window_get_arrangement")
  val WindowGetType            = Value(0x28,  "glk_window_get_type")
  val WindowGetParent          = Value(0x29,  "glk_window_get_parent")
  val WindowClear              = Value(0x2a,  "glk_window_clear")
  val WindowMoveCursor         = Value(0x2b,  "glk_window_move_cursor")
  val WindowGetStream          = Value(0x2c,  "glk_window_get_stream")
  val WindowSetEchoStream      = Value(0x2d,  "glk_window_set_echo_stream")
  val WindowGetEchoStream      = Value(0x2e,  "glk_window_get_echo_stream")
  val SetWindow                = Value(0x2f,  "glk_set_window")
  val WindowGetSibling         = Value(0x30,  "glk_window_get_sibling")
  val StreamIterate            = Value(0x40,  "glk_stream_iterate")
  val StreamGetRock            = Value(0x41,  "glk_stream_get_rock")
  val StreamOpenFile           = Value(0x42,  "glk_stream_open_file")
  val StreamOpenMemory         = Value(0x43,  "glk_stream_open_memory")
  val StreamClose              = Value(0x44,  "glk_stream_close")
  val StreamSetPosition        = Value(0x45,  "glk_stream_set_position")
  val StreamGetPosition        = Value(0x46,  "glk_stream_get_position")
  val StreamSetCurrent         = Value(0x47,  "glk_stream_set_current")
  val StreamGetCurrent         = Value(0x48,  "glk_stream_get_current")
  val FileRefCreateTemp        = Value(0x60,  "glk_fileref_create_temp")
  val FileRefCreateByName      = Value(0x61,  "glk_fileref_create_by_name")
  val FileRefCreateByPrompt    = Value(0x62,  "glk_fileref_create_by_prompt")
  val FileRefDestroy           = Value(0x63,  "glk_fileref_destroy")
  val FileRefIterate           = Value(0x64,  "glk_fileref_iterate")
  val FileRefGetRock           = Value(0x65,  "glk_fileref_get_rock")
  val FileRefDeleteFile        = Value(0x66,  "glk_fileref_delete_file")
  val FileRefDoesFileExist     = Value(0x67,  "glk_fileref_does_file_exist")
  val FileRefCreateFromFileRef = Value(0x68,  "glk_fileref_create_from_fileref")
  val PutChar                  = Value(0x80,  "glk_put_char")
  val PutCharStream            = Value(0x81,  "glk_put_char_stream")
  val PutString                = Value(0x82,  "glk_put_string")
  val PutStringStream          = Value(0x83,  "glk_put_string_stream")
  val PutBuffer                = Value(0x84,  "glk_put_buffer")
  val PutBufferStream          = Value(0x85,  "glk_put_buffer_stream")
  val SetStyle                 = Value(0x86,  "glk_set_style")
  val SetStyleStream           = Value(0x87,  "glk_set_style_stream")
  val GetCharStream            = Value(0x90,  "glk_get_char_stream")
  val GetLineStream            = Value(0x91,  "glk_get_line_stream")
  val GetBufferStream          = Value(0x92,  "glk_get_buffer_stream")
  val CharToLower              = Value(0xa0,  "glk_char_to_lower")
  val CharToUpper              = Value(0xa1,  "glk_char_to_upper")
  val StyleHintSet             = Value(0xb0,  "glk_style_hint_set")
  val StyleHintClear           = Value(0xb1,  "glk_style_hint_clear")
  val StyleDistinguish         = Value(0xb2,  "glk_style_distinguish")
  val StyleMeasure             = Value(0xb3,  "glk_style_measure")
  val Select                   = Value(0xc0,  "glk_select")
  val SelectPoll               = Value(0xc1,  "glk_select_poll")
  val RequestLineEvent         = Value(0xd0,  "glk_request_line_event")
  val CancelLineEvent          = Value(0xd1,  "glk_cancel_line_event")
  val RequestCharEvent         = Value(0xd2,  "glk_request_char_event")
  val CancelCharEvent          = Value(0xd3,  "glk_cancel_char_event")
  val RequestMouseEvent        = Value(0xd4,  "glk_request_mouse_event")
  val CancelMouseEvent         = Value(0xd5,  "glk_cancel_mouse_event")
  val RequestTimerEvents       = Value(0xd6,  "glk_request_timer_events")
  val ImageGetInfo             = Value(0xe0,  "glk_image_get_info")
  val ImageDraw                = Value(0xe1,  "glk_image_draw")
  val ImageDrawScaled          = Value(0xe2,  "glk_image_draw_scaled")
  val WindowFlowBreak          = Value(0xe8,  "glk_window_flow_break")
  val WindowEraseRect          = Value(0xe9,  "glk_window_erase_rect")
  val WindowFillRect           = Value(0xea,  "glk_window_fill_rect")
  val WindowSetBackgroundColor = Value(0xeb,  "glk_window_set_background_color")
  val SChannelIterate          = Value(0xf0,  "glk_schannel_iterate")
  val SChannelGetRock          = Value(0xf1,  "glk_schannel_get_rock")
  val SChannelCreate           = Value(0xf2,  "glk_schannel_create")
  val SChannelDestroy          = Value(0xf3,  "glk_schannel_destroy")
  val SChannelPlay             = Value(0xf8,  "glk_schannel_play")
  val SChannelPlayExt          = Value(0xf9,  "glk_schannel_play_ext")
  val SChannelStop             = Value(0xfa,  "glk_schannel_stop")
  val SChannelSetVolume        = Value(0xfb,  "glk_schannel_set_volume")
  val SoundLoadHint            = Value(0xfc,  "glk_sound_load_hint")
  val SetHyperlink             = Value(0x100, "glk_set_hyperlink")
  val SetHyperlinkStream       = Value(0x101, "glk_set_hyperlink_stream")
  val RequestHyperlinkEvent    = Value(0x102, "glk_request_hyperlink_event")
  val CancelHyperlinkEvent     = Value(0x103, "glk_cancel_hyperlink_event")
  val BufferToLowerCaseUni     = Value(0x120, "glk_buffer_to_lower_case_uni")
  val BufferToUpperCaseUni     = Value(0x121, "glk_buffer_to_upper_case_uni")
  val BufferToTitleCaseUni     = Value(0x122, "glk_buffer_to_title_case_uni")
  val PutCharUni               = Value(0x128, "glk_put_char_uni")
  val PutStringUni             = Value(0x129, "glk_put_string_uni")
  val PutBufferUni             = Value(0x12a, "glk_put_buffer_uni")
  val PutCharStreamUni         = Value(0x12b, "glk_put_char_stream_uni")
  val PutStringStreamUni       = Value(0x12c, "glk_put_string_stream_uni")
  val PutBufferStreamUni       = Value(0x12d, "glk_put_buffer_stream_uni")
  val GetCharStreamUni         = Value(0x130, "glk_get_char_stream_uni")
  val GetBufferStreamUni       = Value(0x131, "glk_get_buffer_stream_uni")
  val GetLineStreamUni         = Value(0x132, "glk_get_line_stream_uni")
  val StreamOpenFileUni        = Value(0x138, "glk_stream_open_file_uni")
  val StreamOpenMemoryUni      = Value(0x139, "glk_stream_open_memory_uni")
  val RequestCharEventUni      = Value(0x140, "glk_request_char_event_uni")
  val RequestLineEventUni      = Value(0x141, "glk_request_line_event_uni")
}

object GestaltSelector {
  val Version              = 0
  val CharInput            = 1
  val LineInput            = 2
  val CharOutput           = 3
  val MouseInput           = 4
  val Timer                = 5
  val Graphics             = 6
  val DrawImage            = 7
  val Sound                = 8
  val SoundVolume          = 9
  val SoundNotify          = 10
  val Hyperlinks           = 11
  val HyperlinkInput       = 12
  val SoundMusic           = 13
  val GraphicsTransparency = 14
  val Unicode              = 15
/*
  val Names = Array("Version", "CharInput", "LineInput", "CharOutput",
                    "MouseInput", "Timer", "Graphics", "DrawImage",
                    "Sound", "SoundVolume", "SoundNotify", "Hyperlinks",
                    "HyperlinkInput", "SoundMusic", "GraphicsTransparency",
                    "Unicode")*/
}

/**
 * The available window types.
 */
object GlkWindowType {
  val All        = 0
  val PairWindow = 1
  val Blank      = 2
  val TextBuffer = 3
  val TextGrid   = 4
  val Graphics   = 5
  //val Names = ("All", "Pair", "Blank", "TextBuffer", "TextGrid", "Graphics")
}

object ImageAlign {
  val InlineUp     = 0x01
  val InlineDown   = 0x02
  val InlineCenter = 0x03
  val MarginLeft   = 0x04
  val MarginRight  = 0x05
  //val Names = ("???", "InlineUp", "InlineDown", "InlineCenter", "MarginLeft", "MarginRight")
}

/**
 * Window positioning constants (part of method bit mask).
 */
object GlkWindowPosition {
  val Left         = 0x00
  val Right        = 0x01
  val Above        = 0x02
  val Below        = 0x03
  val Mask         = 0x0f
  
  def name(method: Int) = (method & Mask) match {
    case Left  => "Left"
    case Right => "Right"
    case Above => "Above"
    case Below => "Below"
    case _     => throw new IllegalArgumentException("unknown window position: %d".format(method & Mask))
  }
}

/**
 * Window division types (part of method bit mask).
 */
object GlkWindowDivision {
  val Fixed        = 0x10
  val Proportional = 0x20
  val Mask         = 0xf0
  
  def name(method: Int) = (method & Mask) match {
    case Fixed        => "Fixed"
    case Proportional => "Proportional"
    case _            => throw new IllegalArgumentException("unknown division: %d".format(method & Mask))
  }
}

/**
 * Window border style (part of winmethod bit mask).
 */
object GlkWindowBorderStyle {
  val Border       = 0x000
  val NoBorder     = 0x100
  val Mask         = 0x100
  def name(method: Int) = (method & Mask) match {
    case Border       => "Border"
    case NoBorder     => "NoBorder"
    case _            => throw new IllegalArgumentException("unknown border style: %d".format(method & Mask))
  }
}

class GlkDimension(var width: Int, var height: Int)
class GlkIterateResult(val id: Int, val rock: Int)
