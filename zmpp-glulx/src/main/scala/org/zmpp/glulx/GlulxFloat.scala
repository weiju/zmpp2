/*
 * Created on 2010/08/12
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
package org.zmpp.glulx

// This object implements most of the Glulx floating point functionality
object GlulxFloat {
  val NegZero = 0x80000000
  val PosInf  = 0x7F800000
  val NegInf  = 0xFF800000

  private def mantissa(intValue: Int) = intValue & 0x7fffff
  private def exponent(intValue: Int) = intValue & 0x7f800000
  def isNaN(intValue: Int) = {
    exponent(intValue) == 0x7f800000 && mantissa(intValue) != 0
  }
  def isInfinity(intValue: Int) = {
    isPositiveInfinity(intValue) || isNegativeInfinity(intValue)
  }
  def isZero(intValue: Int) = intValue == 0 || intValue == NegZero
  def isPositiveNaN(intValue: Int) = intValue >= 0 && isNaN(intValue)
  def isNegativeNaN(intValue: Int) = intValue < 0 && isNaN(intValue)
  def isPositiveInfinity(intValue: Int) = intValue == PosInf
  def isNegativeInfinity(intValue: Int) = intValue == NegInf
  def isNegativeZero(intValue: Int) = intValue == NegZero
  def ftonumz(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    if (isNegativeNaN(intValue) || isNegativeInfinity(intValue)) {
      0x80000000
    } else if (isPositiveNaN(intValue) || isPositiveInfinity(intValue)) {
      0x7FFFFFFF
    } else if (floatValue < 0) {
      scala.math.ceil(floatValue).asInstanceOf[Int]
    } else {
      scala.math.floor(floatValue).asInstanceOf[Int]
    }
  }
  def ftonumn(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    if (isNegativeNaN(intValue) || isNegativeInfinity(intValue)) {
      0x80000000
    } else if (isPositiveNaN(intValue) || isPositiveInfinity(intValue)) {
      0x7FFFFFFF
    } else {
      scala.math.round(floatValue).asInstanceOf[Int]
    }
  }
  def fadd(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    java.lang.Float.floatToRawIntBits(floatValue1 + floatValue2)
  }
  def fsub(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    java.lang.Float.floatToRawIntBits(floatValue1 - floatValue2)
  }
  def fmul(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    java.lang.Float.floatToRawIntBits(floatValue1 * floatValue2)
  }
  def fdiv(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    java.lang.Float.floatToRawIntBits(floatValue1 / floatValue2)
  }
  def fmodQuotient(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    val remainder = floatValue1 % floatValue2
    val quotient = java.lang.Float.floatToRawIntBits(
      (floatValue1 - remainder) / floatValue2)
    if (isZero(quotient)) (intValue1 ^ intValue2) & 0x80000000 
    else quotient
  }
  def fmodRemainder(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    java.lang.Float.floatToRawIntBits(floatValue1 % floatValue2)
  }

  def floor(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.floor(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def ceil(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.ceil(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def sqrt(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.sqrt(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def exp(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.exp(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def log(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.log(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def pow(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    // special cases according to Glulx spec
    if (floatValue1 == 1.0f || floatValue1 == -1.0f && isInfinity(intValue2)) {
      java.lang.Float.floatToRawIntBits(1.0f)
    }
    else {
      val result = scala.math.pow(floatValue1, floatValue2).asInstanceOf[Float]
      java.lang.Float.floatToRawIntBits(result)
    }
  }

  def sin(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.sin(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def cos(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.cos(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def tan(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.tan(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }

  def asin(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.asin(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def acos(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.acos(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def atan(intValue: Int): Int = {
    val floatValue = java.lang.Float.intBitsToFloat(intValue)
    val result = scala.math.atan(floatValue).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }
  def atan2(intValue1: Int, intValue2: Int): Int = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    val result = scala.math.atan2(floatValue1, floatValue2).asInstanceOf[Float]
    java.lang.Float.floatToRawIntBits(result)
  }

  def feq(intValue1: Int, intValue2: Int, intDiff: Int): Boolean = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    val floatDiff = java.lang.Float.intBitsToFloat(intDiff)
    if (isNaN(intValue1) || isNaN(intValue2) || isNaN(intDiff)) false
    else if (isZero(intValue1) && isZero(intValue2)) true
    else if (isNegativeInfinity(intValue1) && isNegativeInfinity(intValue2) ||
             isPositiveInfinity(intValue1) && isPositiveInfinity(intValue2)) true
    else if (isNegativeInfinity(intValue1) && isPositiveInfinity(intValue2) ||
             isPositiveInfinity(intValue1) && isNegativeInfinity(intValue2)) false
    else if (isInfinity(intDiff)) true
    else if (isZero(intDiff)) intValue1 == intValue2
    else {
      val eps = scala.math.abs(floatDiff)
      scala.math.abs(floatValue1 - floatValue2) <= eps
    }
  }
  def fne(intValue1: Int, intValue2: Int, intDiff: Int): Boolean = {
    !feq(intValue1, intValue2, intDiff)
  }
  def flt(intValue1: Int, intValue2: Int): Boolean = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    if (isZero(intValue1) && isZero(intValue2)) false
    else floatValue1 < floatValue2
  }
  def fle(intValue1: Int, intValue2: Int): Boolean = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    if (isZero(intValue1) && isZero(intValue2)) true
    else floatValue1 <= floatValue2
  }
  def fgt(intValue1: Int, intValue2: Int): Boolean = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    if (isZero(intValue1) && isZero(intValue2)) false
    else floatValue1 > floatValue2
  }
  def fge(intValue1: Int, intValue2: Int): Boolean = {
    val floatValue1 = java.lang.Float.intBitsToFloat(intValue1)
    val floatValue2 = java.lang.Float.intBitsToFloat(intValue2)
    if (isZero(intValue1) && isZero(intValue2)) true
    else floatValue1 >= floatValue2
  }
}
