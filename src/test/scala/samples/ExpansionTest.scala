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
  
}

@Test
class ExpansionTest {
  
  import ExamplePol._
  
  val atts = Set(subject.roles)
  val converter = new TreeConverter(testPol1, atts)
  val pdp = new PDP(testPol1, new AttributeFinder)

  @Test
  def expansionTest() = {
    
  }
  
  @Test
  def canBeSplitTest() = {
    assert(converter.canBeSplit(testPol1) == true)
  }
  
  @Test
  def splitPolicyTest() = {
    
  }
  
  @Test
  def splitRulesTest() = {
    
  }
  
  @Test
  def splitTest() = {
    
  }
  
  @Test
  def findCommonTest() = {
    assert(converter.findCommon(testPol1.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol1.subpolicies(1).asInstanceOf[stapl.core.Rule], atts).isInstanceOf[ValueIn])
    val newAtts = Set(subject.location)
    assert(converter.findCommon(testPol1.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol1.subpolicies(1).asInstanceOf[stapl.core.Rule], newAtts).isInstanceOf[And])
  }
  
  @Test
  def findCommonsTest() = {
     assert(converter.findCommon(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol2.subpolicies(1).asInstanceOf[stapl.core.Rule], atts).isInstanceOf[ValueIn])
    val newAtts = Set(subject.location)
    println(converter.findCommon(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol2.subpolicies(1).asInstanceOf[stapl.core.Rule], newAtts))
    assert(converter.findCommon(testPol2.subpolicies(0).asInstanceOf[stapl.core.Rule],
        testPol2.subpolicies(1).asInstanceOf[stapl.core.Rule], newAtts).isInstanceOf[And])
        //TODO fix failure in code
  }
  
  @Test
  def removeCommonTest() = {
    //TODO common removen en testen of pdp steeds zelfde evalueert
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
  
}