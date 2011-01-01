/*
 * Created on 2010/11/04
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
package org.zmpp.glulx

import java.util.logging._

class ChannelIOSystem(vm: GlulxVM, rock: Int) extends IOSystem(vm, rock) {
  def id = 20
  def streamChar(c: Char) {
    logger.info("streamChar(%c)".format(c))
  }
  def streamUniChar(c: Int) {
    logger.info("streamUniChar(%c)".format(c.asInstanceOf[Char]))
  }
  
  // streamstr actions
  def handleChar8(c: Char, inBetween: Boolean, currentStreamByte: Int,
                  currentStreamBit: Int)    = {
    logger.info("handleChar8(%c)".format(c))
    StreamStrState.Continue
  }
  def handleChar32(c: Int, inBetween: Boolean, currentStreamByte: Int,
                   currentStreamBit: Int) = {
    logger.info("handleChar32(%d)".format(c))
    StreamStrState.Continue
  }
  def handleHuffmanCString(nodeAddr: Int,
                           currentStreamByte: Int, currentStreamBit: Int,
                           inBetween: Boolean): StreamStrState = {
    logger.info("handleHuffmanCString(%04x)".format(nodeAddr))
    StreamStrState.Continue
  }
  def handleHuffmanUnicodeString(nodeAddr: Int,
                                 currentStreamByte: Int, currentStreamBit: Int,
                                 inBetween: Boolean): StreamStrState = {
    logger.info("handleHuffmanUnicodeString(%04x)".format(nodeAddr))
    StreamStrState.Continue
  }
}
