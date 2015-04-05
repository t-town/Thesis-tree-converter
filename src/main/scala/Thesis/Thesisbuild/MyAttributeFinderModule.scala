package Thesis.Thesisbuild

import stapl.core.pdp.AttributeFinderModule
import stapl.core.AttributeContainerType
import stapl.core.ConcreteValue
import stapl.core.AttributeType
import stapl.core.pdp.EvaluationCtx

class MyAttributeFinderModule extends AttributeFinderModule {
  
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    println("Yeeey")
    return null //TODO coden voor tests sleep here
  }
  

}