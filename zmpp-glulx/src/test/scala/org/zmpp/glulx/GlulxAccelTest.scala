/*
 * Created on 2010/04/28
 * Copyright (c) 2010-2014, Wei-ju Wu.
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

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.BeforeAndAfterEach

import org.zmpp.glk.Glk

@RunWith(classOf[JUnitRunner])
class AccelSystemSpec extends FlatSpec with Matchers with BeforeAndAfterEach {
  var vm: GlulxVM = null
  var accelSys: AccelSystem = null

  override def beforeEach {
    vm = new GlulxVM
    accelSys = new AccelSystem(vm)
  }

  "AccelSystem" should "be initialized" in {
    accelSys.isAccelerated(100) should be (false)
  }
  it should "accelerate a function" in {
    accelSys.setFunction(1, 100)
    accelSys.isAccelerated(100) should be (true)
  }
  it should "cancel a function acceleration" in {
    accelSys.setFunction(1, 100)
    accelSys.setFunction(0, 100)
    accelSys.isAccelerated(100) should be (false)
  }
  it should "redefine a function acceleration" in {
    accelSys.setFunction(1, 100)
    accelSys.setFunction(2, 100)
    accelSys.isAccelerated(100) should be (true)
    // test if the mapping is different from before (TODO)
  }
}
