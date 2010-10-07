/*
 * Created on 2010/10/06
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

import scala.collection.mutable.HashMap

// Define a very simple TADS3 object class for now, which later holds all
// object types. At the moment only static objects
class Tads3Object(staticObject: StaticObject) {
  def findProperty(propertyId: Int) = staticObject.findProperty(propertyId)
  def dump {
    printf("TADS3 OBJECT: %s\n", staticObject.toString)
  }
}

class MetaClass {
}

// The object manager handles instantiation and management of objects
class ObjectManager {
  private var _maxObjectId = 0
  private var _metaClassDependencies: Array[MetaClassDependency] = null
  private var _staticObjects        : HashMap[Int, StaticObject] = null

  def reset(metaClassDeps: Array[MetaClassDependency],
            staticObjs: HashMap[Int, StaticObject],
            maxObjectId: Int) {
    _maxObjectId           = maxObjectId
    _metaClassDependencies = metaClassDeps

    // TODO: initialize the collection of current objects with static objects
    _staticObjects         = staticObjs
  }

  def newId = {
    _maxObjectId += 1
    _maxObjectId
  }
  def objectWithId(id: Int)    = {
    // TODO: Retrieve the objects from a global object collection
    new Tads3Object(_staticObjects(id))
  }
}
