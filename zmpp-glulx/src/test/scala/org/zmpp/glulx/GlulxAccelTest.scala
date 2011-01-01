/*
 * Created on 2010/04/28
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

import org.specs._
import org.specs.matcher._
import org.specs.runner.{ConsoleRunner, JUnit4}

import org.zmpp.glk.Glk

class GlulxAccelTest extends JUnit4(AccelSystemSpec)
object AccelSystemSpecRunner extends ConsoleRunner(AccelSystemSpec)

object AccelSystemSpec extends Specification with xUnit {
  "AccelSystem" should {
    var vm: GlulxVM = null
    var accelSys: AccelSystem = null

    doBefore {
      vm = new GlulxVM
      accelSys = new AccelSystem(vm)
    }
    "be initialized" in {
      assertFalse(accelSys.isAccelerated(100))
    }
    "accelerate a function" in {
      accelSys.setFunction(1, 100)
      assertTrue(accelSys.isAccelerated(100))
    }
    "cancel a function acceleration" in {
      accelSys.setFunction(1, 100)
      accelSys.setFunction(0, 100)
      assertFalse(accelSys.isAccelerated(100))
    }
    "redefine a function acceleration" in {
      accelSys.setFunction(1, 100)
      accelSys.setFunction(2, 100)
      assertTrue(accelSys.isAccelerated(100))
      // test if the mapping is different from before (TODO)
    }
  }
}
