package Thesis.Thesisbuild

import stapl.core._
import stapl.templates.general.GeneralTemplates
import stapl.performance.Timer
import stapl.core.pdp.PDP
import org.joda.time.LocalDateTime
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.RequestCtx

object Experiment {
  
  import ExperimentPolicy._
  import samples.TestExample._
  import java.io._
  
  val finder = new AttributeFinder
  finder += new MyAttributeFinderModule
  val pdp = new PDP(TestPol , finder)
  
  val converter = new TreeConverter(testPolcopy, Set.empty)
  converter.convertTree();
  val pdp2 = new PDP(converter.root, finder)
  
  def main(args: Array[String]) {
  val tNormal = new Timer()
  val tConvert = new Timer()
  
  var l1,l2,l3,l4,l5,l6,l7,l8,l9,l10=0
  
  //Measuring number of accessed attributes before conversion
    l1 = test1(pdp).employedAttributes.size
    l2 = test2(pdp).employedAttributes.size
    l3 = test3(pdp).employedAttributes.size
    l4 = test4(pdp).employedAttributes.size
    l5 = test5(pdp).employedAttributes.size
    l6 = test6(pdp).employedAttributes.size
    l7 = test7(pdp).employedAttributes.size
    l8 = test8(pdp).employedAttributes.size
    l9 = test9(pdp).employedAttributes.size
   l10 = test10(pdp).employedAttributes.size

  //Measuring evaluation time before conversion
  for(i <- 1 to 100){
    tNormal.time(test1(pdp))
    tNormal.time(test2(pdp))
    tNormal.time(test3(pdp))
    tNormal.time(test4(pdp))
    tNormal.time(test5(pdp))
    tNormal.time(test6(pdp))
    tNormal.time(test7(pdp))
    tNormal.time(test8(pdp))
    tNormal.time(test9(pdp))
    tNormal.time(test10(pdp))
  }
  
  //write results before conversion
  val file = new File("normalAccess.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(l1.toString + "\n")
  bw.write(l2.toString + "\n")
  bw.write(l3.toString + "\n")
  bw.write(l4.toString + "\n")
  bw.write(l5.toString + "\n")
  bw.write(l6.toString + "\n")
  bw.write(l7.toString + "\n")
  bw.write(l8.toString + "\n")
  bw.write(l9.toString + "\n")
  bw.write(l10.toString + "\n")
  bw.close()
  
  val file1 = new File("normalTime.txt")
  val bw1 = new BufferedWriter(new FileWriter(file1))
  for(t <- tNormal.timings)
	  bw1.write(t + "\n")
  bw1.close()
  
  //Measuring number of accessed attributes after conversion
    l1 = test1(pdp2).employedAttributes.size
    l2 = test2(pdp2).employedAttributes.size
    l3 = test3(pdp2).employedAttributes.size
    l4 = test4(pdp2).employedAttributes.size
    l5 = test5(pdp2).employedAttributes.size
    l6 = test6(pdp2).employedAttributes.size
    l7 = test7(pdp2).employedAttributes.size
    l8 = test8(pdp2).employedAttributes.size
    l9 = test9(pdp2).employedAttributes.size
   l10 = test10(pdp2).employedAttributes.size
   
    //Measuring evaluation time after conversion
  for(i <- 1 to 100){
    tConvert.time(test1(pdp2))
    tConvert.time(test2(pdp2))
    tConvert.time(test3(pdp2))
    tConvert.time(test4(pdp2))
    tConvert.time(test5(pdp2))
    tConvert.time(test6(pdp2))
    tConvert.time(test7(pdp2))
    tConvert.time(test8(pdp2))
    tConvert.time(test9(pdp2))
    tConvert.time(test10(pdp2))
  }
  
   //write results before conversion
  val file2 = new File("convertAccess.txt")
  val bw2 = new BufferedWriter(new FileWriter(file2))
  bw2.write(l1.toString + "\n")
  bw2.write(l2.toString + "\n")
  bw2.write(l3.toString + "\n")
  bw2.write(l4.toString + "\n")
  bw2.write(l5.toString + "\n")
  bw2.write(l6.toString + "\n")
  bw2.write(l7.toString + "\n")
  bw2.write(l8.toString + "\n")
  bw2.write(l9.toString + "\n")
  bw2.write(l10.toString + "\n")
  bw2.close()
  
  val file3 = new File("convertTime.txt")
  val bw3 = new BufferedWriter(new FileWriter(file3))
  for(t <- tConvert.timings)
	  bw3.write(t + "\n")
  bw3.close()
}
  
  def test1(pdp : PDP) = pdp.evaluate("test1", "view", "doc123")
  
  def test2(pdp : PDP) = pdp.evaluate("test2", "view", "doc123")
  
  def test3(pdp : PDP) = pdp.evaluate("test3", "view", "doc123")
  
  def test4(pdp : PDP) = pdp.evaluate("test4", "view", "doc123")
  
  def test5(pdp : PDP) = pdp.evaluate("test5", "view", "doc123")
  
  def test6(pdp : PDP) = pdp.evaluate("test6", "view", "doc123")
  
  def test7(pdp : PDP) = pdp.evaluate("test7", "view", "doc123")
  
  def test8(pdp : PDP) = pdp.evaluate("test8", "view", "doc123")
  
  def test9(pdp : PDP) = pdp.evaluate("test9", "view", "doc123")
  
  def test10(pdp : PDP) = pdp.evaluate("test10", "view", "doc123")

}

object ExperimentPolicy extends BasicPolicy with GeneralTemplates {
  
  import stapl.core.dsl._
  
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

  // The policy set for "view patient status".
  val testPolcopy = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (    
    // The consent policy.
    Policy("policy:1") := when ("medical_personnel" in subject.roles) apply PermitOverrides to (
        Rule("consent") := deny iff (subject.id in resource.owner_withdrawn_consents),
        Rule("breaking-glass") := permit iff (subject.triggered_breaking_glass) performing (log(subject.id + " performed breaking-the-glass procedure"))
    ) performing (log("permit because of breaking-the-glass procedure") on Permit),
    
    // Only physicians, nurses and patients can access the monitoring system.
    Rule("policy:2") := deny iff !(("nurse" in subject.roles) | ("physician" in subject.roles) | ("patient" in subject.roles)),
    
    // For physicians.
    Policy("policyset:2") := when ("physician" in subject.roles) apply FirstApplicable to (      
      // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
      Rule("policy:3") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency") | ("gp" in subject.roles)),
      
      // All of the previous physicians except for the GPs can access the monitoring system in case of emergency.
      Rule("policy:4") := permit iff (((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency"))
                                      & (subject.triggered_breaking_glass | resource.operator_triggered_emergency | resource.indicates_emergency)),
      
      // For cardiologists.
      Policy("policyset:4") := when (subject.department === "cardiology") apply PermitOverrides to (        
        // Permit for head physician.
        Rule("policy:7") := permit iff (subject.is_head_physician),
        
        // Permit if treated the patient or treated in team.
        Rule("policy:8") := permit iff (resource.owner_id in subject.treated) | (resource.owner_id in subject.treated_by_team),
        
        Rule("policy:9") := deny
      ),
      
      // For physicians of elder care department: only permit if admitted in care unit or treated in the last six months.
      OnlyPermitIff("policyset:5")(subject.department === "elder_care")(
          (resource.owner_id in subject.admitted_patients_in_care_unit)
          | (resource.owner_id in subject.treated_in_last_six_months)
      )
    ),
    
    // For nurses.
    Policy("policyset:7") := when ("nurse" in subject.roles) apply FirstApplicable to (      
      // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
      Rule("policy:14") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care")),
      
      // Nurses can only access the PMS during their shifts.
      Rule("policy:15") := deny iff !((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)),
      
      // Nurses can only access the PMS from the hospital.
      Rule("policy:16") := deny iff !(subject.location === "hospital"),
      
      // Nurses can only view the patient's status of the last five days.
      Rule("policy:17") := deny iff !(environment.currentDateTime lteq (resource.created + 5.days)),
      
       // For nurses of the elder care department.
      Policy("policyset:9") := when (subject.department === "elder_care") apply DenyOverrides to (
        // Of the nurses of the elder care department, only nurses who have been allowed to use the PMS can access the PMS.
        Rule("policy:20") := deny iff !subject.allowed_to_access_pms,
        
        // Nurses of the elder care department can only view the patient status of a patient 
        // who is currently admitted to their nurse unit and for whome they are assigned responsible.
        OnlyPermitIff("policySet:10")(AlwaysTrue)(
            (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
            	& (resource.owner_id in subject.responsible_patients)
        )
      )
    ),
    // For patients
    Policy("policyset:11") := when ("patient" in subject.roles) apply FirstApplicable to (      
	      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
    	  // subscribed to the PMS, but is not paying any more).
	      Rule("policy:23") := deny iff !subject.allowed_to_access_pms,
	      
	      // A patient can only view his own status.
	      Rule("policy:24") := deny iff !(resource.owner_id === subject.id),
	      
	      Rule("policy:25") := permit
    )
  )
}