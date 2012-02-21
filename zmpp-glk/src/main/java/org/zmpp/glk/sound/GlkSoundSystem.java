/*
 * Created on 2012/02/20
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
package org.zmpp.glk.sound;

import java.util.logging.*;
import java.util.List;
import java.util.ArrayList;

import org.zmpp.iff.*;
import org.zmpp.glk.GlkIterateResult;

public class GlkSoundSystem {
    private static Logger logger = Logger.getLogger("glk");
    private List<GlkSoundChannel> channels = new ArrayList<GlkSoundChannel>();
    private int _nextId = 1;
    public NativeSoundSystem nativeSoundSystem;

    private GlkSoundChannel channelWithId(int id) {
        for (GlkSoundChannel channel: channels) {
            if (channel.id == id) return channel;
        }
        return NullSoundChannel.getInstance();
    }

    public void destroyChannel(int channelId) {
        GlkSoundChannel channel = channelWithId(channelId);
        channel.stop();
        channels.remove(channel);
    }

    public GlkIterateResult iterate(int channelId) {
        GlkSoundChannel channel = null;
        if (!channels.isEmpty()) {
            if (channelId == 0) channel = channels.get(0);
            else {
                int i = 0;
                for (; i < channels.size(); i++) {
                    if (channels.get(i).id == channelId) break;
                }
                channel = (i >= channels.size() - 1) ? null : channels.get(i + 1);
            }
        }
        return (channel == null) ? new GlkIterateResult(0, 0) : new GlkIterateResult(channel.id, channel.rock);
    }
    
    public void stopChannel(int channelId) { channelWithId(channelId).stop(); }
  
    public int createChannel(int rock) {
        NativeSoundChannel nativeChannel = nativeSoundSystem.createChannel();
        GlkSoundChannel newChannel = new GlkSoundChannel(_nextId++, rock, nativeChannel);
        channels.add(newChannel);
        return newChannel.id;
    }
  
    public void setVolume(int channelId, int volume) {
        channelWithId(channelId).setVolume(volume);
    }
  
    public int play(int channelId, int soundnum, int repeats, int notify) {
        return channelWithId(channelId).play(soundnum, repeats, notify) ? 1 : 0;
    }

    public int getRock(int channelId) { return channelWithId(channelId).rock; }
}
