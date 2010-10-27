/*
 * Created on 2010/10/09
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

import org.zmpp.base._

/*
 * An anonymous function ptr. Interestingly, this inherits from Vector.
 * Why ? This is because there is not only the code offset, but also
 * an arbitrary number of context objects.
 * As a nice side effect, this automatically implements the indexed access.
 */
class AnonFuncPtr(id: TadsObjectId, metaClass: MetaClass)
extends Vector(id, metaClass) {
  override def findProperty(propertyId: Int): Property = {
    val objectCallProp = metaClass.vmState.image.symbolicNames("ObjectCallProp")
    if (propertyId == objectCallProp.value) {
      val propValue = valueAtIndex(1)
      new Property(propertyId, propValue, id)
    } else super.findProperty(propertyId)
  }
}

class AnonFuncPtrMetaClass extends MetaClass {
  def name = "anon-func-ptr"
  override def superMeta = objectSystem.metaClassForName("vector")

  override def createFromImage(objectId: TadsObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): TadsObject = {
    val anonFuncPtr = new AnonFuncPtr(objectId, this)
    anonFuncPtr
  }

  override def createFromStack(id: TadsObjectId, argc: Int) = {
    if (argc < 1) {
      throw new IllegalArgumentException("%s: createFromStack() needs at " +
                                         "least 1 " +
                                         "parameters.".format(name))
    }
    val functionPtr = vmState.stack.pop
    //printf("argc: %d, Function Ptr: %s\n", argc, functionPtr)
    if (functionPtr.valueType != TypeIds.VmFuncPtr) {
      throw new IllegalArgumentException("%s: createFromStack() with illegal " +
                                         "argument type: %d\n".format(
                                         name, functionPtr.valueType))
    }
    val result = new AnonFuncPtr(id, this)
    result.add(functionPtr)
    // copy (argc - 1) context objects into the result object
    for (i <- 1 until argc) result.add(vmState.stack.pop)
    result
  }
}
