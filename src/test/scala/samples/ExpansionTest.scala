package samples

import org.junit._
import Assert._
import stapl.core._
import stapl.examples._
import stapl.templates.general.GeneralTemplates
import Thesis.Thesisbuild.TreeConverter
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder

object ExamplePol extends BasicPolicy with GeneralTemplates{
  
  environment.currentDateTime = SimpleAttribute(DateTime)
  resource.type_ = SimpleAttribute(String)
  resource.owner_withdrawn_consents = ListAttribute(String)
  resource.operator_triggered_emergency = SimpleAttribute(Bool)
  resource.indicates_emergency = SimpleAttribute(Bool)
  resource.owner_id = SimpleAttribute("owner:id", String)
  resource.owner_responsible_physicians = ListAttribute("owner:responsible_physicians", String)
  resource.owner_discharged = SimpleAttribute("owner:discharged", Bool)
  resource.owner_discharged_dateTime = SimpleAttribute("owner:discharged_dateTime", DateTime)
  resource.patient_status = SimpleAttribute(String)
  resource.created = SimpleAttribute(DateTime)
  subject.roles = ListAttribute(String)
  subject.triggered_breaking_glass = SimpleAttribute(Bool)
  subject.department = SimpleAttribute(String)
  subject.current_patient_in_consultation = SimpleAttribute(String)
  subject.treated_in_last_six_months = ListAttribute(String)
  subject.primary_patients = ListAttribute(String)
  subject.is_head_physician = SimpleAttribute(Bool)
  subject.treated = ListAttribute(String)
  subject.treated_by_team = ListAttribute(String)
  subject.admitted_patients_in_care_unit = ListAttribute(String)
  subject.shift_start = SimpleAttribute(DateTime)
  subject.shift_stop = SimpleAttribute(DateTime)
  subject.location = SimpleAttribute(String)
  subject.admitted_patients_in_nurse_unit = ListAttribute(String)
  subject.allowed_to_access_pms = SimpleAttribute(Bool)
  subject.responsible_patients = ListAttribute(String)
  
  val testPol1 = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to ( 
      stapl.core.Rule("DNF-1") := deny iff ("nurse" in subject.roles) | 
          ((subject.location === "hospital") & (subject.department === "elder_care")) |
          ((subject.department === "cardiology")),
      stapl.core.Rule("DNF-2") := permit iff ("nurse" in subject.roles) | 
          ((subject.location === "hospital") & (subject.department === "elder_care")) |
          (!(subject.department === "cardiology"))
      )
  
  val testPol2 = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to ( 
      stapl.core.Rule("DNF-1") := deny iff ("nurse" in subject.roles) | 
          ((subject.location === "hospital") & (subject.department === "elder_care")) |
          ((subject.department === "cardiology")),
      stapl.core.Rule("DNF-2") := permit iff ("nurse" in subject.roles) | 
          ((subject.department === "elder_care") & (subject.location === "hospital")) |
          (!(subject.department === "cardiology"))
      )
  
  val testPol3 = null //TODO test expand and
  
}

@Test
class ExpansionTest {
  
  import ExamplePol._
  
  val atts = Set(subject.roles)
  val converter = new TreeConverter(testPol1, atts)
  val pdp = new PDP(testPol1, new AttributeFinder)

  @Test
  def expansionTest() = {
    var asin = testPol1.asInstanceOf[Policy]
    var copyrules:List[AbstractPolicy] = List.empty
    for(r <- asin.subpolicies){
      var asinr = r.asInstanceOf[stapl.core.Rule]
      copyrules ::= new stapl.core.Rule(asinr.id)(asinr.effect,asinr.condition,List.empty)
    }
    var copy = new Policy(asin.id)(asin.target,asin.pca,copyrules,List.empty)
    var newPol = converter.expand(copy)
    
    var ev1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.location -> "hospital",
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    val npdp = new PDP(newPol, new AttributeFinder)
    
    var ev2 = npdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.location -> "hospital",
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    assert(ev1 == ev2)
  }
  
  @Test
  def canBeSplitTest() = {
    assert(converter.canBeSplit(testPol1,atts) == true)
  }
  
  @Test
  def findCommonTest() = {
    assert(converter.findCommon(testPol1.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol1.subpolicies(1).asInstanceOf[stapl.core.Rule], atts, "Or").isInstanceOf[ValueIn])
    val newAtts = Set(subject.location)
    assert(converter.findCommon(testPol1.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol1.subpolicies(1).asInstanceOf[stapl.core.Rule], newAtts, "Or").isInstanceOf[And])
  }
  
  @Test
  def findCommonsTest() = {
     assert(converter.findCommon(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol2.subpolicies(1).asInstanceOf[stapl.core.Rule], atts, "Or").isInstanceOf[ValueIn])
    val newAtts = Set(subject.location)
    println("Testpol2")
    println(converter.findCommon(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol2.subpolicies(1).asInstanceOf[stapl.core.Rule], newAtts, "Or"))
    assert(converter.findCommon(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol2.subpolicies(1).asInstanceOf[stapl.core.Rule], newAtts, "Or").isInstanceOf[And])
  }
  
  @Test
  def removeCommonTest() = {
    var common = converter.findCommon(testPol1.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol1.subpolicies(1).asInstanceOf[stapl.core.Rule], atts, "Or")
    var newSubs:List[stapl.core.Rule] = List.empty
    for(s <- testPol1.subpolicies){ 
      var ns = s.asInstanceOf[stapl.core.Rule]
      var remove = converter.removeCommon(ns.condition, common)
      var newrule = new stapl.core.Rule(s.id)(ns.effect,remove,List.empty)
      newSubs ::= newrule
      }
    
   var newPol = new Policy(testPol1.asInstanceOf[Policy].id)(testPol1.asInstanceOf[Policy].target,
        testPol1.asInstanceOf[Policy].pca,newSubs,List.empty)
    
    var ev1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.location -> "hospital",
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    val npdp = new PDP(newPol, new AttributeFinder)
    
    var ev2 = npdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.location -> "hospital",
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    assert(ev1 == ev2)
  }
  
  @Test
  def splitOrTest() = {
    assert(converter.splitOr(testPol1.subpolicies(0).asInstanceOf[stapl.core.Rule].condition).size == 3)
    assert(converter.splitOr(testPol1.subpolicies(1).asInstanceOf[stapl.core.Rule].condition).size == 3)
  }
  
  @Test
  def nbKnownAttributesTest() = {
    assert(converter.nbKnownAttributes(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule].condition, atts)
        ==1)
  }
  
  @Test
  def expandAndTest() = {
    //TODO test
  }
  
}