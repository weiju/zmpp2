/*
 * Created on 2010/08/12
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
package org.zmpp.glulx;

// This object implements most of the Glulx floating point functionality
class GlulxFloat {

    public static final int NegZero = 0x80000000;
    public static final int PosInf  = 0x7F800000;
    public static final int NegInf  = 0xFF800000;

    private static int mantissa(int intValue) { return intValue & 0x007fffff; }
    private static int exponent(int intValue) { return intValue & 0x7f800000; }
    protected static boolean isNaN(int intValue) {
        return exponent(intValue) == 0x7f800000 && mantissa(intValue) != 0;
    }
    protected static boolean isInfinity(int intValue) {
        return isPositiveInfinity(intValue) || isNegativeInfinity(intValue);
    }

    public static boolean isZero(int intValue) { return intValue == 0 || intValue == NegZero; }
    public static boolean isPositiveNaN(int intValue) { return intValue >= 0 && isNaN(intValue); }
    public static boolean isNegativeNaN(int intValue) { return intValue < 0 && isNaN(intValue); }
    public static boolean isPositiveInfinity(int intValue) { return intValue == PosInf; }
    public static boolean isNegativeInfinity(int intValue) { return intValue == NegInf; }
    public static boolean isNegativeZero(int intValue) { return intValue == NegZero; }

    public static int ftonumz(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        if (isNegativeNaN(intValue) || isNegativeInfinity(intValue)) {
            return 0x80000000;
        } else if (isPositiveNaN(intValue) || isPositiveInfinity(intValue)) {
            return 0x7FFFFFFF;
        } else if (floatValue < 0) {
            return (int) Math.ceil(floatValue);
        } else {
            return (int) Math.floor(floatValue);
        }
    }

    public static int ftonumn(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        if (isNegativeNaN(intValue) || isNegativeInfinity(intValue)) {
            return 0x80000000;
        } else if (isPositiveNaN(intValue) || isPositiveInfinity(intValue)) {
            return 0x7FFFFFFF;
        } else {
            return (int) Math.round(floatValue);
        }
    }

    public static int fadd(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        return Float.floatToRawIntBits(floatValue1 + floatValue2);
    }

    public static int fsub(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        return Float.floatToRawIntBits(floatValue1 - floatValue2);
    }

    public static int fmul(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        return Float.floatToRawIntBits(floatValue1 * floatValue2);
    }

    public static int fdiv(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        return Float.floatToRawIntBits(floatValue1 / floatValue2);
    }

    public static int fmodQuotient(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        float remainder = floatValue1 % floatValue2;
        int quotient = Float.floatToRawIntBits((floatValue1 - remainder) / floatValue2);
        if (isZero(quotient)) return (intValue1 ^ intValue2) & 0x80000000;
        else return quotient;
    }

    public static int fmodRemainder(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        return Float.floatToRawIntBits(floatValue1 % floatValue2);
    }

    public static int floor(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.floor(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int ceil(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.ceil(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int sqrt(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.sqrt(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int exp(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.exp(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int log(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.log(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int pow(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        // special cases according to Glulx spec
        if (floatValue1 == 1.0f || floatValue1 == -1.0f && isInfinity(intValue2)) {
            return Float.floatToRawIntBits(1.0f);
        } else {
            float result = (float) Math.pow(floatValue1, floatValue2);
            return Float.floatToRawIntBits(result);
        }
    }

    public static int sin(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.sin(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int cos(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.cos(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int tan(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.tan(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int asin(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.asin(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int acos(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.acos(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int atan(int intValue) {
        float floatValue = Float.intBitsToFloat(intValue);
        float result = (float) Math.atan(floatValue);
        return Float.floatToRawIntBits(result);
    }

    public static int atan2(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        float result = (float) Math.atan2(floatValue1, floatValue2);
        return Float.floatToRawIntBits(result);
    }

    public static boolean feq(int intValue1, int intValue2, int intDiff) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        float floatDiff = Float.intBitsToFloat(intDiff);

        if (isNaN(intValue1) || isNaN(intValue2) || isNaN(intDiff))              return false;
        else if (isZero(intValue1) && isZero(intValue2))                         return true;
        else if (isNegativeInfinity(intValue1) && isNegativeInfinity(intValue2) ||
                 isPositiveInfinity(intValue1) && isPositiveInfinity(intValue2)) return true;
        else if (isNegativeInfinity(intValue1) && isPositiveInfinity(intValue2) ||
                 isPositiveInfinity(intValue1) && isNegativeInfinity(intValue2)) return false;
        else if (isInfinity(intDiff))                                            return true;
        else if (isZero(intDiff))                                                return intValue1 == intValue2;
        else {
            float eps = Math.abs(floatDiff);
            return Math.abs(floatValue1 - floatValue2) <= eps;
        }
    }

    public static boolean fne(int intValue1, int intValue2, int intDiff) {
        return !feq(intValue1, intValue2, intDiff);
    }

    public static boolean flt(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        if (isZero(intValue1) && isZero(intValue2)) return false;
        else return floatValue1 < floatValue2;
    }

    public static boolean fle(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        if (isZero(intValue1) && isZero(intValue2)) return true;
        else return floatValue1 <= floatValue2;
    }

    public static boolean fgt(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        if (isZero(intValue1) && isZero(intValue2)) return false;
        else return floatValue1 > floatValue2;
    }

    public static boolean fge(int intValue1, int intValue2) {
        float floatValue1 = Float.intBitsToFloat(intValue1);
        float floatValue2 = Float.intBitsToFloat(intValue2);
        if (isZero(intValue1) && isZero(intValue2)) return true;
        else return floatValue1 >= floatValue2;
    }
}
