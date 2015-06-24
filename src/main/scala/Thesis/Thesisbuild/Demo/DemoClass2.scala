package Thesis.Thesisbuild.Demo

import stapl.templates.general.GeneralTemplates
import stapl.core._
import stapl.core.pdp.AttributeFinderModule
import stapl.core.pdp.EvaluationCtx
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.PDP
import Thesis.Thesisbuild.Experiment.TreeConverter
import stapl.performance.Timer

object Demo2 {
  
  import DemoTree2._
  
  val finder = new AttributeFinder
  finder += new DemoAttFinder2
  val pdp = new PDP(demotree2 , finder)
  
  val pdp2 = new PDP(demotree2alt, finder)
  
  def main(args: Array[String]) {
    val tNormal = new Timer()
    val tConvert = new Timer()
    
    for(i <- 1 to 1000){
      tNormal.time(pdp.evaluate("UserX","view","demo.txt"))
    }
    
    for(i <- 1 to 1000){
      tConvert.time(pdp2.evaluate("UserX","view","demo.txt"))
    }
    
    println("======Originele boom======")
    println("Evaluatietijd: " + tNormal.mean + " ms")
    println("Aantal attributen: " + pdp.evaluate("UserX","view","demo.txt").employedAttributes.size)
    println()
    printTree(demotree2)
    println()
    println("======Getransformeerde boom======")
    println("Evaluatietijd: " + tConvert.mean + " ms")
    println("Aantal attributen: " + pdp2.evaluate("UserX","view","demo.txt").employedAttributes.size)
    println()
    printTree(demotree2alt)
  }
  
  def printTree(p:Policy):Unit = {
    printTree(p,0)
  }
  
  def printTree(a:AbstractPolicy,space:Int):Unit = a match {
    	case p:Policy => printPolicy(p,space)
    	case r:Rule => printRule(r,space)
  }
  
  def printPolicy(p:Policy,space:Int) = {
    printSpace(space)
    print("Policy "+p.id+ ": when " + p.target + " apply " + p.pca + " to \n")
    for(c <- p.subpolicies)
      printTree(c,space+1)
  }
  
  def printRule(r:Rule,space:Int) = {
    printSpace(space)
    print("Rule "+r.id+ ": " + r.effect + " iff " + r.condition + "\n")
  }
  
  def printSpace(space:Int):Unit = {
    if(space>0){
      print("|")
      printSpace(space-1)
    }
  }
  
}

object DemoTree2 extends BasicPolicy with GeneralTemplates {
  
  import stapl.core.dsl._
  
  environment.werknemer_ids = ListAttribute(String)
  subject.rollen = ListAttribute(String)
  resource.type_ = SimpleAttribute(String)
  
  val demotree2 = 
    Policy("P1") := when (subject.id in environment.werknemer_ids) apply PermitOverrides to (
      Policy("P2") := when("HR" in subject.rollen) apply FirstApplicable to(
       Rule("R1") := permit iff (resource.type_ === "HR_bestand"),
       Rule("R2") := deny
      ),
      Rule("R3") := permit iff ("IT" in subject.rollen),
      Rule("R4") := deny
    )
  
  val demotree2alt = 
    Policy("P1") := when (subject.id in environment.werknemer_ids) apply FirstApplicable to (
       Rule("R1") := permit iff ("IT" in subject.rollen)|(("HR" in subject.rollen) & (resource.type_ === "HR_bestand")),
       Rule("R2") := deny 
    )
}

class DemoAttFinder2 extends AttributeFinderModule {
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    Thread.sleep(2)
    cType match {
      case SUBJECT => name match {
        case "rollen" => Some(List("IT","Manager"))
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