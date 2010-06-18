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
import org.zmpp.iff._

trait NativeSoundChannel {
  def play(soundnum: Int, repeats: Int, notify: Boolean): Boolean
  def setVolume(volume: Int)
  def stop  
}

trait NativeSoundSystem {
  def createChannel: NativeSoundChannel
}

class GlkSoundChannel(val id: Int, val rock: Int, nativeChannel: NativeSoundChannel) {
  val logger = Logger.getLogger("glk")
  
  def stop = nativeChannel.stop
  def setVolume(volume: Int) {
    nativeChannel.setVolume(volume)
  }

  def play(soundnum: Int, repeats: Int, notify: Boolean): Boolean = {
    nativeChannel.play(soundnum, repeats, notify)
  }  
}

object NullSoundChannel extends GlkSoundChannel(0, 0, null) {
  override def stop { }
  override def setVolume(volume: Int) { }
  override def play(soundnum: Int, repeats: Int, notify: Boolean) = false
}


class GlkSoundSystem {
  val logger = Logger.getLogger("glk")
  private var channels: List[GlkSoundChannel] = Nil
  private var _nextId = 1
  var nativeSoundSystem: NativeSoundSystem = null
  
  // Iteration is done a little differently than for the other iterable
  // objects: It seems that the Scala 2.8.0 compiler has a strange bug with
  // forward references.
  // By doing iteration in this way, we work around the problem
  private def makeIterateResult(soundChannel: GlkSoundChannel): GlkIterateResult = {
    if (soundChannel != null) {
      new GlkIterateResult(soundChannel.id, soundChannel.rock)
    } else new GlkIterateResult(0, 0)
  }
  private def _iterate(channelId: Int) = {
    if (channels.isEmpty) null
    else if (channelId == 0) channels.head
    else {
      val remain = channels.dropWhile(channel => channel.id != channelId).tail
      if (remain.isEmpty) null
      else remain.head 
    }
  }
  private def nextId = {
    _nextId += 1
    _nextId - 1
  }

  private def channelWithId(id: Int) = {
    if (channels.isEmpty) NullSoundChannel
    else channels.filter(channel => channel.id == id).head
  }

  def destroyChannel(channelId: Int) {
    channelWithId(channelId).stop
    channels = channels.filterNot(channel => channel.id == channelId)
  }

  def iterate(channelId: Int): GlkIterateResult = {
    makeIterateResult(_iterate(channelId))
  }
  
  def stopChannel(channelId: Int) = channelWithId(channelId).stop
  
  def createChannel(rock: Int): Int = {
    val nativeChannel = nativeSoundSystem.createChannel
    val newChannel = new GlkSoundChannel(nextId, rock, nativeChannel)
    channels ::= newChannel
    newChannel.id
  }
  
  def setVolume(channelId: Int, volume: Int) {
    channelWithId(channelId).setVolume(volume)
  }
  
  def play(channelId: Int, soundnum: Int, repeats: Int, notify: Boolean): Int = {
    if (channelWithId(channelId).play(soundnum, repeats, notify)) 1 else 0
  }

  def getRock(channelId: Int) = channelWithId(channelId).rock
}

