package Thesis.Thesisbuild

import stapl.core._
import stapl.core.pdp._
import stapl.examples.policies.EhealthPolicy

/**
 * @author ${user.name}
 */
object App {
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
  import ExamplePolicy._
  val ehealth = EhealthPolicy.naturalPolicy
  val pdp = new PDP(ehealth)
  val converter = new TreeConverter(ehealth, null)
  //test shit here
  var policyreduce = converter.reduce(ehealth)
  println(policyreduce.subpolicies.length)
  for(p <- policyreduce.subpolicies) {println(p.isInstanceOf[Rule]);println(p.toString())}
  println(policyreduce.parent.toString())
  /*println(pdp.evaluate("subject1", "view", "resource1", 
          subject.roles -> List("physician"))) // will return Permit
  println(pdp.evaluate("subject1", "view", "resource1", 
          subject.roles -> List("another role"))) // will return Deny     
  println(pdp.evaluate("subject1", "another action", "resource1", 
          subject.roles -> List("physician"))) // will return NotApplicable */
}

}


object ExamplePolicy extends BasicPolicy {

  subject.roles = ListAttribute(String)

  val policy = Policy("example policy") := when (action.id === "view") apply PermitOverrides to (
    Rule("permit physicians") := permit iff ("physician" in subject.roles) ,
    Rule("default deny") := deny
  )  
  
  val chainpolicy = Policy("chain policy") := when (action.id === "view") apply FirstApplicable to (
    Rule("permit physicians") := permit iff ("physician" in subject.roles) ,
    Rule("permit otters") := deny iff ("nurse" in subject.roles),
    Rule("keep testing") := permit,
    Rule("default deny") := deny
  )  
  
  val testpolicy = Policy("example policy") := when (action.id === "view") apply PermitOverrides to (
    Rule("test1 physicians") := permit iff ("physician" in subject.roles) ,
    Rule("test1 deny") := deny,
    Policy("testpolicy") := when (action.id === "view") apply PermitOverrides to (
    		Rule("test2 physicians") := permit iff ("physician" in subject.roles) ,
    		Rule("test2 deny") := deny,
    		Policy("Deeptest") := when (action.id === "view") apply DenyOverrides to (
    		    Rule("deeptest1") := deny
    		    )
    )
  )  
}
