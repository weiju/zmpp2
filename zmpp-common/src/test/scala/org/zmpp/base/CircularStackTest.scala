/*
 * Created on 2011/11/10
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
package org.zmpp.base

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._

@RunWith(classOf[JUnitRunner])
class CircularStackSpec extends FlatSpec with Matchers {

  "CicularStack" should "be initialized" in {
    val stack = new CircularStack[Int](2)
    stack.empty should be (true)
  }
  it should "push an element" in {
    val stack = new CircularStack[Int](2)
    stack.push(1)
    stack.empty should be (false)
  }
  it should "pop the last pushed element" in {
    val stack = new CircularStack[Int](2)
    stack.push(1)
    stack.push(2)
    stack.empty should be (false)
    stack.pop should be (2)
    stack.empty should be (false)
    stack.pop should be (1)
    stack.empty should be (true)
  }

  it should "forget the previous elements if capacity exceeded" in {
    val stack = new CircularStack[Int](2)
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.push(4)
    stack.empty should be (false)
    stack.pop should be (4)
    stack.empty should be (false)
    stack.pop should be (3)
    stack.empty should be (true)
  }
}
