package samples

import org.junit._
import Assert._
import stapl.core._
import stapl.templates.general.GeneralTemplates
import Thesis.Thesisbuild.Experiment.TreeConverter
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import org.joda.time.LocalDateTime

object ExamplePolicy extends BasicPolicy with GeneralTemplates{
  
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
    
    val testPO = OnlyPermitIff("policySet:10")(AlwaysTrue)(
            (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
            	& (resource.owner_id in subject.responsible_patients)
        )
        
    val testDO = Policy("policyset:11") := when ("nurse" in subject.roles) apply DenyOverrides to (      
	      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
    	  // subscribed to the PMS, but is not paying any more).
	      Rule("policy:23") := deny iff !subject.allowed_to_access_pms,
	      
	      // A patient can only view his own status.
	      Rule("policy:16") := deny iff !(subject.location === "hospital"),
	      
	      Rule("policy:25") := permit
    )
  
  val testFA = Policy("policyset:11") := when ("nurse" in subject.roles) apply FirstApplicable to (      
	      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
    	  // subscribed to the PMS, but is not paying any more).
	      Rule("policy:23") := deny iff !subject.allowed_to_access_pms,
	      
	      Rule("policy:25") := permit
    )
  
  val testPhysicians = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (  
    Policy("policyset:2") := when ("physician" in subject.roles) apply FirstApplicable to (      
      // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
      Rule("policy:3") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency") | ("gp" in subject.roles)),
      
      // All of the previous physicians except for the GPs can access the monitoring system in case of emergency.
      Rule("policy:4") := permit iff (((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency"))
                                      & (subject.triggered_breaking_glass | resource.operator_triggered_emergency | resource.indicates_emergency)),
      
      // For GPs: only permit if in consultation or treated in the last six months or primary physician or responsible in the system.
      OnlyPermitIff("policyset:3")("gp" in subject.roles)(
          (resource.owner_id === subject.current_patient_in_consultation)
          | (resource.owner_id in subject.treated_in_last_six_months)
          | (resource.owner_id in subject.primary_patients)
          | (subject.id in resource.owner_responsible_physicians)
      ),
      
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
      ),
      
      // For physicians of emergency department: only permit if patient status is bad (or the above).
      OnlyPermitIff("policyset:6")(subject.department === "emergency")(   
          resource.patient_status === "bad"
      )
    )
  )
  val testNurses = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (  
    Policy("policyset:7") := when ("nurse" in subject.roles) apply FirstApplicable to (      
      // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
      Rule("policy:14") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care")),
      
      // Nurses can only access the PMS during their shifts.
      Rule("policy:15") := deny iff !((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)),
      
      // Nurses can only access the PMS from the hospital.
      Rule("policy:16") := deny iff !(subject.location === "hospital"),
      
      // Nurses can only view the patient's status of the last five days.
      Rule("policy:17") := deny iff !(environment.currentDateTime lteq (resource.created + 5.days)),
      
      // For nurses of cardiology department: they can only view the patient status of a patient 
      // in their nurse unit for whom they are assigned responsible, up to three days after they were discharged.
      OnlyPermitIff("policyset:8")(subject.department === "cardiology")(
          (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
          	& (!resource.owner_discharged | (environment.currentDateTime lteq (resource.owner_discharged_dateTime + 3.days)))
      ),
        
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
    )
  )
  val testPatients = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (  
    Policy("policyset:11") := when ("patient" in subject.roles) apply FirstApplicable to (      
	      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
    	  // subscribed to the PMS, but is not paying any more).
	      Rule("policy:23") := deny iff !subject.allowed_to_access_pms,
	      
	      // A patient can only view his own status.
	      Rule("policy:24") := deny iff !(resource.owner_id === subject.id),
	      
	      Rule("policy:25") := permit
    )
    )
  
  val testOthers = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (    
    // The consent policy.
    Policy("policy:1") := when ("medical_personnel" in subject.roles) apply PermitOverrides to (
        Rule("consent") := deny iff (subject.id in resource.owner_withdrawn_consents),
        Rule("breaking-glass") := permit iff (subject.triggered_breaking_glass) performing (log(subject.id + " performed breaking-the-glass procedure"))
    ) performing (log("permit because of breaking-the-glass procedure") on Permit),
    
    // Only physicians, nurses and patients can access the monitoring system.
    Rule("policy:2") := deny iff !(("nurse" in subject.roles) | ("physician" in subject.roles) | ("patient" in subject.roles))

    )
  
  val testDNF = Policy("DNF-parent") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (    
      //A&(B|C)&D
      Rule("DNF-1") := deny iff (("nurse" in subject.roles) & 
          ((subject.department === "cardiology") | (subject.department === "elder_care")) & 
          (subject.location === "hospital")),
      //A&(!B|C)&D
      Rule("DNF-2") := permit iff (("nurse" in subject.roles) & 
          (!(subject.department === "cardiology") | (subject.department === "elder_care")) &
          (subject.location === "hospital"))
     )
  }

@Test
class conversionTest{

  import ExamplePolicy._
  
  @Test
  def testPOtoDO() {
    val converter = new TreeConverter(testPO, null)
    var pdp = new PDP(converter.convertCA(testPO, DenyOverrides), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        
    pdp = new PDP(testPO, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    assert(res1 == res2)
    
  }
  @Test
  def testPOtoFA() {
    val converter = new TreeConverter(testPO, null)
    var pdp = new PDP(converter.convertCA(testPO, FirstApplicable), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        
    pdp = new PDP(testPO, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    assert(res1 == res2)
    
  }
  
  @Test
  def testDOtoPO() {
    val converter = new TreeConverter(testDO, null)
    var pdp = new PDP(converter.convertCA(converter.reduceLeaves(testDO), PermitOverrides), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        
    pdp = new PDP(testDO, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    assert(res1 == res2)
    
  }
  @Test
  def testDOtoFA() {
    val converter = new TreeConverter(testDO, null)
    var pdp = new PDP(converter.convertCA(testDO, FirstApplicable), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        
    pdp = new PDP(testDO, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    assert(res1 == res2)
    
  }
  
  @Test
  def testFAtoPO() {
    val converter = new TreeConverter(testFA, null)
    var pdp = new PDP(converter.convertCA(converter.reduceLeaves(testFA), PermitOverrides), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        
    pdp = new PDP(testFA, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    assert(res1 == res2)
    
  }
  @Test
  def testFAtoDO() {
    val converter = new TreeConverter(testFA, null)
    var pdp = new PDP(converter.convertCA(testFA, DenyOverrides), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        
    pdp = new PDP(testFA, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    assert(res1 == res2)
    
  }
  
  @Test
  def PhysicianTest() {
    var pdp = new PDP(testPhysicians, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
        println("Physician test: " + res2.toString())
    val converter = new TreeConverter(testPhysicians, null)
    pdp = new PDP(converter.reduce(testPhysicians), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    assert(res1 == res2)
  }
  
  @Test
  def NurseTest() {
    var pdp = new PDP(testNurses, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    val converter = new TreeConverter(testNurses, null)
    pdp = new PDP(converter.reduce(testNurses), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
   
    println("Nurse test: " + res2.toString())   
    assert(res1 == res2)
    
  }
  
  @Test
  def PatientTest() {
    var pdp = new PDP(testPatients, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    val converter = new TreeConverter(testPatients, null)
    pdp = new PDP(converter.reduce(testPatients), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
     
    
    assert(res1 == res2)
    println("Patient test: " + res2.toString())
  }
  
   @Test
  def OtherTest() {
    var pdp = new PDP(testOthers, new AttributeFinder)
    var res2 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
        
    val converter = new TreeConverter(testOthers, null)
    pdp = new PDP(converter.reduce(testOthers), new AttributeFinder)
    var res1 = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
     
    println("Other test: " + res2.toString())
    assert(res1 != res2) //because of obligations
  }
   
   @Test
   def DNFtest() {
     val converter = new TreeConverter(testDNF, null)
     val p = converter.normalise(testDNF)
     assert(converter.index == 0)
     assert(converter.propMap.size == 0)
   }
  
}