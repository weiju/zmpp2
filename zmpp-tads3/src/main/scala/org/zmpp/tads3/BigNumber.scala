/*
 * Created on 2010/10/14
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
package org.zmpp.tads3

import scala.collection.JavaConversions._
import java.util.ArrayList
import org.zmpp.base._

// We store a BigNumber value as a varying-length string of BCD-encoded
// digits; we store two digits in each byte.  Our bytes are stored from
// most significant to least significant, and each byte has the more
// significant half in the high part of the byte.

// UINT2 number_of_digits
// INT2 exponent
// BYTE flags
// BYTE most_significant_byte
// ...
// BYTE least_significant_byte

// Note that the number of bytes of the varying length mantissa string
// is equal to (number_of_digits+1)/2, because one byte stores two
// digits.

// The flags are:

// (flags & 0x0001) - sign bit; zero->non-negative, nonzero->negative

// (flags & 0x0006):
// 0x0000 -> normal number
// 0x0002 -> NOT A NUMBER
// 0x0004 -> INFINITY (sign bit indicates sign of infinity)
// 0x0006 -> reserved - should always be zero for now

// (flags & 0x0008) - zero bit; if set, the number's value is zero

// All other flag bits are reserved and should be set to zero.

// The exponent field gives the base-10 exponent of the number.  This is
// a signed quantity; a negative value indicates that the mantissa is tk

// that the mantissa is to be multiplied by (10 ^ exponent).

// There is an implicit decimal point before the first byte of the
// mantissa.
class BigNumber(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {
  def metaClass = objectSystem.bigNumberMetaClass
}

class BigNumberMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "bignumber"
  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    new BigNumber(objectId, vmState, isTransient)
  }
}
