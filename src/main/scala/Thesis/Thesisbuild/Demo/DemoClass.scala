package Thesis.Thesisbuild.Demo

import stapl.templates.general.GeneralTemplates
import stapl.core._
import stapl.core.pdp.AttributeFinderModule
import stapl.core.pdp.EvaluationCtx
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.PDP
import Thesis.Thesisbuild.Experiment.TreeConverter
import stapl.performance.Timer

object Demo {
  
  import DemoTree._
  
  val finder = new AttributeFinder
  finder += new DemoAttFinder
  val pdp = new PDP(demotree , finder)
  
  val converter = new TreeConverter(demotreeCopy)
  converter.convertTree();
  val pdp2 = new PDP(converter.root, finder)
  
  def main(args: Array[String]) {
    val tNormal = new Timer()
    val tConvert = new Timer()
    
    for(i <- 1 to 200){
      tNormal.time(pdp.evaluate("UserX","view","demo.txt"))
    }
    
    for(i <- 1 to 200){
      tConvert.time(pdp2.evaluate("UserX","view","demo.txt"))
    }
    
    println("======Originele boom======")
    println("Evaluatietijd: " + tNormal.mean + " ms")
    println("Aantal attributen: " + pdp.evaluate("UserX","view","demo.txt").employedAttributes.size)
    println()
    println("======Getransformeerde boom======")
    println("Evaluatietijd: " + tConvert.mean + " ms")
    println("Aantal attributen: " + pdp2.evaluate("UserX","view","demo.txt").employedAttributes.size) 
  }
  
}

object DemoTree extends BasicPolicy with GeneralTemplates {
  
  import stapl.core.dsl._
  
  environment.werknemer_ids = ListAttribute(String)
  subject.rollen = ListAttribute(String)
  resource.type_ = SimpleAttribute(String)
  
  val demotree = 
    Policy("P1") := when (subject.id in environment.werknemer_ids) apply PermitOverrides to (
      Policy("P2") := when("HR" in subject.rollen) apply FirstApplicable to(
       Rule("R1") := permit iff (resource.type_ === "HR_bestand"),
       Rule("R2") := deny
      ),
      Rule("R3") := permit iff ("IT" in subject.rollen),
      Rule("R4") := deny
    )
  
  val demotreeCopy = 
    Policy("P1") := when (subject.id in environment.werknemer_ids) apply PermitOverrides to (
      Policy("P2") := when("HR" in subject.rollen) apply FirstApplicable to(
       Rule("R1") := permit iff (resource.type_ === "HR_bestand"),
       Rule("R2") := deny
      ),
      Rule("R3") := permit iff ("IT" in subject.rollen),
      Rule("R4") := deny
    )
}

class DemoAttFinder extends AttributeFinderModule {
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    Thread.sleep(2)
    cType match {
      case SUBJECT => name match {
        case "rollen" => Some(List("HR","Manager"))
        case _ => None
      }
      case RESOURCE => name match {
        case "type_" => Some("HR_bestand")
        case _ => None
      }
      case ENVIRONMENT => name match{
        case "werknemer_ids" => Some(List("UserX","UserY","UserZ")) 
        case _ => None
      }
      case _ => None
    }
  }
}