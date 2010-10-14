/*
 * Created on 2010/10/12
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

import java.util.ArrayList
// treat Java collections like Scala collections
import scala.collection.JavaConversions._

// GenericObjects are instances of what the documentation calls "TADS Object".
// We wanted to avoid confusion, because "TadsObject" is the super class of
// all object classes in the ZMPP implementation. "GenericObjects" seems like
// a more fitting name which also more accurately reflects the purpose as
// a very flexible data structure the user can manipulate.
// This implementation currently just delegates to a static object for
// most cases, which is fine for now, but is not pure on-demand loading.
// We won't provide a different approach until for file-based loading,
// because of the need to enumerate all objects with some functions.
// We keep in mind that the generic object has almost a 1:1 representation
// in the load/save image.
class GenericObject(id: TadsObjectId, objectManager: ObjectManager)
extends AbstractTadsObject(id) {
  var staticObject: StaticObject = null
  override def isTransient = staticObject.isTransient
  override def findProperty(propertyId: Int):Property = {
    val prop = staticObject.findProperty(propertyId)
    if (prop != null) return prop
    // not found in object try super class properties
    for (i <- 0 until staticObject.superClassCount) {
      val superClassId = staticObject.superClassIdAt(i)
      printf("not found, try super class: %d\n", superClassId)
      val superClass = objectManager.objectWithId(superClassId)
      val prop = superClass.findProperty(propertyId)
      if (prop != null) return prop
    }
    null
  }
}

class GenericObjectMetaClass extends SystemMetaClass {
  def name = "tads-object"
  override def createFromImage(staticObject: StaticObject,
                               objectManager: ObjectManager): TadsObject = {
    val genericObject = new GenericObject(new TadsObjectId(staticObject.id),
                                          objectManager)
    genericObject.staticObject = staticObject
    genericObject
  }
}
