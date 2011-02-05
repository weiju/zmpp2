package org.zmpp.tads3

import T3Assert._

class CharacterSet(id: T3ObjectId, vmState: TadsVMState, isTransient: Boolean,
                   charsetName: String)
extends AbstractT3Object(id, vmState, isTransient) {
  protected def staticMetaClass = objectSystem.characterSetMetaClass
  def metaClass = objectSystem.characterSetMetaClass

  override def getProperty(propertyId: Int, argc: Int): Property = {
    val idx = staticMetaClass.functionIndexForProperty(propertyId)
    printf("collection prop idx = %d\n", idx)
    val prop = staticMetaClass.callMethodWithIndex(this, idx, argc)
    if (prop != InvalidPropertyId) new Property(propertyId, prop, id)
    else super.getProperty(propertyId, argc)
  }
}

class CharacterSetMetaClass(objectSystem: ObjectSystem)
extends AbstractMetaClass(objectSystem) {
  def name = "character-set"
  val FunctionVector = Array(undef _,      getName _,             isMappingKnown _,
                             isMappable _, isRoundTripMappable _)

  def undef(obj: T3Object, argc: Int): T3Value = InvalidPropertyId
  def getName(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("getName")
  }
  def isMappingKnown(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isMappingKnown")
  }
  def isMappable(obj: T3Object, argc: Int): T3Value = {
    throw new UnsupportedOperationException("isMappable")
  }
  def isRoundTripMappable(obj: T3Object, argc: Int): T3Value = {
    argc must_== 1
    vmState.stack.pop // pop the argument
    T3True
  }

  override def createFromStack(id: T3ObjectId, argc: Int,
                               isTransient: Boolean) = {
    argc must_== 1
    val charsetName = objectSystem.toTadsString(vmState.stack.pop).string
    new CharacterSet(id, vmState, isTransient, charsetName)
  }

  override def callMethodWithIndex(obj: T3Object, index: Int,
                                   argc: Int): T3Value = {
    FunctionVector(index)(obj, argc)
  }
}
