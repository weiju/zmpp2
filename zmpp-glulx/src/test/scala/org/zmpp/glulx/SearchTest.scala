/*
 * Created on 2010/04/01
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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

import java.io._
import org.zmpp.base._

object SearchSpec {
  // A simple memory setup with key size 1 byte
  val ArraySearchMemSize1 = Array[Byte](0x47, 0x6c, 0x75, 0x6c, // Glul
                                        0x00, 0x03, 0x01, 0x01, // SIZE
                                        0x00, 0x00, 0x00, 0x00, // RAMSTART
                                        0x00, 0x00, 0x01, 0x00, // EXTSTART
                                        0x00, 0x00, 0x02, 0x00, // ENDMEM
                                        0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                                        0x00, 0x00, 0x00, 0x00, // STARTFUNC
                                        0x00, 0x00, 0x00, 0x00, // Decoding table
                                        0x00, 0x00, 0x00, 0x00, // Checksum
                                        0x01, 0x03, 0x04, 0x07  // 36 -> Search Data
                                      )

  // A simple search memory setup with key size 1
  val LinkedSearchMemSize1 = Array[Byte](0x47, 0x6c, 0x75, 0x6c, // Glul
                                         0x00, 0x03, 0x01, 0x01, // SIZE
                                         0x00, 0x00, 0x00, 0x00, // RAMSTART
                                         0x00, 0x00, 0x01, 0x00, // EXTSTART
                                         0x00, 0x00, 0x02, 0x00, // ENDMEM
                                         0x00, 0x00, 0x00, 0xff.asInstanceOf[Byte], // STACKSIZE
                                         0x00, 0x00, 0x00, 0x00, // STARTFUNC
                                         0x00, 0x00, 0x00, 0x00, // Decoding table
                                         0x00, 0x00, 0x00, 0x00, // Checksum
                                         0x01, 0x00, 0x00, 0x00, // 36 -> search data
                                         0x29, 0x03, 0x00, 0x00, // 40
                                         0x00, 0x00, 0x00, 0x00  // 44
                                       )
}

@RunWith(classOf[JUnitRunner])
class BinarySearchSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var vmstate = new GlulxVMState

  override def beforeEach {
    vmstate.init(SearchSpec.ArraySearchMemSize1)
  }

  "BinarySearch" should "search keys returning key address" in {
    val binarySearch1 = new BinarySearch(vmstate, key=1, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=0)
    binarySearch1.search should be (36)
    val binarySearch2 = new BinarySearch(vmstate, key=3, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=0)
    binarySearch2.search should be (37)
  }
  it should "search keys returning index" in {
    val binarySearch1 = new BinarySearch(vmstate, key=1, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=4)
    binarySearch1.search should be (0)
    val binarySearch2 = new BinarySearch(vmstate, key=7, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=4)
    binarySearch2.search should be (3)
  }
}

@RunWith(classOf[JUnitRunner])
class LinearSearchSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var vmstate = new GlulxVMState

  override def beforeEach {
    vmstate.init(SearchSpec.ArraySearchMemSize1)
  }

  "LinearSearch" should "search keys returning key address" in {
    val linearSearch1 = new LinearSearch(vmstate, key=1, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=0)
    linearSearch1.search should be (36)
    val linearSearch2 = new LinearSearch(vmstate, key=3, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=0)
    linearSearch2.search should be (37)
  }
  it should "search keys returning index" in {
    val linearSearch1 = new LinearSearch(vmstate, key=1, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=4)
    linearSearch1.search should be (0)
    val linearSearch2 = new LinearSearch(vmstate, key=7, keySize=1, start=36,
                                         structSize=1, numStructs=4, keyOffset=0,
                                         options=4)
    linearSearch2.search should be (3)
  }
}

@RunWith(classOf[JUnitRunner])
class LinkedSearchSpec extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {

  var vmstate = new GlulxVMState

  override def beforeEach {
    vmstate.init(SearchSpec.LinkedSearchMemSize1)
  }

  "LinkedSearch" should "search keys returning key address" in {
    val linkedSearch1 = new LinkedSearch(vmstate, key=1, keySize=1, start=36,
                                         keyOffset=0, nextOffset=1, options=0)
    linkedSearch1.search should be (36)
    val linkedSearch2 = new LinkedSearch(vmstate, key=3, keySize=1, start=36,
                                         keyOffset=0, nextOffset=1, options=0)
    linkedSearch2.search should be (41)
  }
}
