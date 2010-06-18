/*
 * Created on 2010/06/08
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
package org.zmpp.glulx.swing

import java.io.InputStream
import javax.sound.sampled._
import javax.sound.sampled.spi._

import java.util.logging._
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Callable
import java.util.concurrent.Future

import org.zmpp.iff._
import org.zmpp.glk._
import org.zmpp.glulx._


/*
 * A factory object for creating AudioInputStream objects.
 * Unfortunately, the JOrbis and MuLine SPIs are conflicting with the
 * AIFF SPI that comes in JavaSE, so we need to create the input streams
 * by hand. The most dubious aspect of this is using com.sun.media classes,
 * which is not available in OpenJDK. Explicitly calling the SPIs to create
 * the streams kind of defeats the SPI idea, this factory handles the conflict
 * resolution by retrieving the resource type from the Blorb file and then
 * calling the correct SPI.
 */
object AudioStreamFactory {
  val logger = Logger.getLogger("zmppsound")
  var aiffReader:      AudioFileReader = null
  var oggVorbisReader: AudioFileReader = null
  var modReader:       AudioFileReader = null


  aiffReader = createReaderInstance("com.sun.media.sound.AiffFileReader", "AIFF")
  oggVorbisReader =
    createReaderInstance("javazoom.spi.vorbis.sampled.file.VorbisAudioFileReader", "Vorbis")
  modReader = createReaderInstance("org.muforge.musound.muxm.spi.ModuleFileReader", "MOD")
  
  private def createReaderInstance(className: String, soundType: String): AudioFileReader = {
    try {
      val readerClass = Class.forName(className)
      return readerClass.newInstance().asInstanceOf[AudioFileReader]
    } catch {
      case _ => logger.warning("No %s reader found in the classpath (library missing)".format(soundType))
    }
    null
  }

  def createAudioInputStream(soundChunkId: String,
                             inputStream: InputStream): AudioInputStream = {
    val reader: AudioFileReader =
      if (soundChunkId == "FORM") aiffReader
      else if (soundChunkId == "OGGV") oggVorbisReader
      else if (soundChunkId == "MOD ") modReader
      else null
    if (reader == null) null else reader.getAudioInputStream(inputStream)
  }
}

class PlaySoundTask(blorbData: BlorbData, channel: LineListener, 
                    soundnum: Int, repeats: Int)
extends Callable[Boolean] {
  val logger = Logger.getLogger("zmppsound")
  
  @volatile
  var running = true

  def call: Boolean = {
    running = true
    playStream(soundnum)
  }

  private def audioInputStream(soundnum: Int): AudioInputStream = {
    val inputStream: InputStream = blorbData.soundInputStream(soundnum)
    val resourceInfo = blorbData.soundResource(soundnum)
    val subChunk = blorbData.formChunk.chunkAtAddress(resourceInfo.start)
    AudioStreamFactory.createAudioInputStream(subChunk.id, inputStream)
  }

  private def playStream(soundnum: Int): Boolean = {
    val in = audioInputStream(soundnum)
    val baseFormat = in.getFormat
    val decodedFormat = new AudioFormat(
      AudioFormat.Encoding.PCM_SIGNED,
      baseFormat.getSampleRate,
      16,
      baseFormat.getChannels,
      baseFormat.getChannels * 2,
      baseFormat.getSampleRate,
      false)
    val decodedInputStream = AudioSystem.getAudioInputStream(decodedFormat, in)
    playDecoded(decodedFormat, decodedInputStream)
    in.close
    true
  }

  private def playDecoded(targetFormat: AudioFormat, decodedIn: AudioInputStream) {
    val data = new Array[Byte](4096)
    val line = getLine(targetFormat)
    if (line != null) {
      line.addLineListener(channel)
      line.start
      var nBytesRead = 0
      var nBytesWritten = 0
      while (running && nBytesRead != -1) {
        nBytesRead = decodedIn.read(data, 0, data.length)
        if (nBytesRead != -1) nBytesWritten = line.write(data, 0, nBytesRead)
      }
      // remove the line listener before the line.stop call to prevent the
      // stop event being sent to the channel
      if (!running) {
        line.removeLineListener(channel)
        logger.info("PLAY TASK INTERRUPTED !!!")
      }
      line.drain
      line.stop
      line.close
      line.removeLineListener(channel)
      decodedIn.close
    }
  }
  
  private def getLine(audioFormat: AudioFormat): SourceDataLine = {
    var result: SourceDataLine = null
    val info = new DataLine.Info(classOf[SourceDataLine], audioFormat)
    result = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    result.open(audioFormat)
    result
  }
  
  def stop {
    running = false
  }
}

class JavaSeSoundChannel(blorbData: BlorbData)
extends NativeSoundChannel with LineListener {
  val logger = Logger.getLogger("zmppsound")
  var currentTask: PlaySoundTask = null
  var currentFuture: Future[Boolean] = null
  val executor = Executors.newSingleThreadExecutor
  var notifyOnStop = false

  def play(soundnum: Int, repeats: Int, notify: Boolean): Boolean = {
    logger.info("SoundChannel.play(%d) repeats: %d".format(soundnum, repeats))
    notifyOnStop = notify
    stop
    try {
      if (blorbData.soundResource(soundnum) != null) {
        currentTask = new PlaySoundTask(blorbData, this, soundnum, repeats)
        currentFuture = executor.submit(currentTask)
        return true
      } else return false
    } catch {
      case ex => ex.printStackTrace
    }
    false
  }
  
  def setVolume(volume: Int) {
    logger.info("SoundChannel.setVolume(%d)".format(volume))
  }
  def stop {
    //logger.info("SoundChannel.stop")
    if (currentFuture != null && !currentFuture.isDone) {
      // interrupt previous sound playing if necessary. We do not wait for
      // the task to finish, to improve reponsiveness
      currentTask.stop
    }
  }
  def update(event: LineEvent) {
    if (event.getType == LineEvent.Type.STOP) {
      logger.info("SOUND STOPPED !!")
    }
  }
}

class JavaSeSoundSystem(blorbData: BlorbData) extends NativeSoundSystem {
  def createChannel: NativeSoundChannel = {
    new JavaSeSoundChannel(blorbData)
  }
}

