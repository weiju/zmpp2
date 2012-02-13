/*
 * Created on 2011/02/12
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
package org.zmpp.glulx;

public class GlulxGestalt {

    public static final int Version      = 0;
    public static final int TerpVersion  = 1;
    public static final int ResizeMem    = 2;
    public static final int Undo         = 3;
    public static final int IOSystem     = 4;
    public static final int Unicode      = 5;
    public static final int MemCopy      = 6;
    public static final int MAlloc       = 7;
    public static final int MAllocHeap   = 8;
    public static final int Acceleration = 9;
    public static final int AccelFunc    = 10;
    public static final int GlulxFloat   = 11;

  public static int gestalt(int selector, int param) {
      System.out.printf("Glulx.gestalt(#$%02x, #$%02x)\n", selector, param);
      switch (selector) {        
      case 0:  return 0x00030102; // Version
      case 1:  return 0x00010000; // TerpVersion
      case 2:  return 1;          // ResizeMem
      case 3:  return 1;          // Undo
      case 4:                    // IOSystem, suppose we know FyreVM
          return (param >= 0 && param <= 2 || param == 20) ? 1 : 0;
      case 5:  return 1;          // Unicode
      case 6:  return 1;          // MemCopy
      case 7:  return 1;          // MAlloc
      case 9:  return 1; // Acceleration
      case 10: return (param >= 1 && param <= 7) ? 1 : 0; // AccelFunc
      case 11: return 1; // GlulxFloat
      default: return 0;
    }
  }
}
