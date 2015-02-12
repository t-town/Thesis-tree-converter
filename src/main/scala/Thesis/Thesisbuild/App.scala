package Thesis.Thesisbuild

import stapl.core._
import stapl.core.pdp._

/**
 * @author ${user.name}
 */
object App {
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
  import ExamplePolicy._
  val pdp = new PDP(policy)
  val converter = new TreeConverter(testpolicy, null)
  //test shit here
  var policies = converter.getLowestPolicies(testpolicy)
  for(p <- policies) println(p.toString())
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
  
  val testpolicy = Policy("example policy") := when (action.id === "view") apply PermitOverrides to (
    Rule("test1 physicians") := permit iff ("physician" in subject.roles) ,
    Rule("test1 deny") := deny,
    Policy("testpolicy") := when (action.id === "view") apply PermitOverrides to (
    		Rule("test2 physicians") := permit iff ("physician" in subject.roles) ,
    		Rule("test2 deny") := deny
    )
  )  
}
