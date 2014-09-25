/*
 * Created on 2010/10/14
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
package org.zmpp.tads3

import scala.collection.JavaConversions._
import java.util.ArrayList
import org.zmpp.base._

// Grammar production object - image file format

// UINT2 alt_count
// alternative 1
// alternative 2
// etc

// Each alternative has the following structure:

// INT2 score
// INT2 badness
// UINT4 processor_object_id
// UINT2 token_count
// token 1
// token 2
// etc

// Each token has this structure:

// UINT2 property_association
// BYTE token_match_type (see below)
// extra data depending on token_match_type (see below)

// The extra data for the token varies by match type:

// VMGRAM_MATCH_PROD - a UINT4 giving the production object ID

// VMGRAM_MATCH_SPEECH - a UINT2 giving the vocabulary property

// VMGRAM_MATCH_NSPEECH - a UINT2 giving a count, then that many
// additional UINT2's giving a list of vocabulary properties

// VMGRAM_MATCH_LITERAL - a UINT2 byte-length prefix followed by the
// UTF8-encoded bytes of the literal string

// VMGRAM_MATCH_TOKTYPE - a UINT4 giving the token enum's ID

// VMGRAM_MATCH_STAR - no additional data 

class GrammarProduction(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean)
extends AbstractT3Object(id, vmState, isTransient) {
  def metaClass = objectSystem.grammarProductionMetaClass
}

class GrammarProductionMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "grammar-production"
  override def createFromImage(objectId: T3ObjectId,
                               objDataAddr: Int,
                               numBytes: Int,
                               isTransient: Boolean): T3Object = {
    new GrammarProduction(objectId, vmState, isTransient)
  }
}
