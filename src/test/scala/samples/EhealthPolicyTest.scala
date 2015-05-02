/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package samples

import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit
import org.joda.time.LocalDateTime
import Thesis.Thesisbuild.TreeConverter
import Thesis.Thesisbuild.MyAttributeFinderModule
import stapl.examples.policies.EhealthPolicy
import stapl.templates.general.GeneralTemplates
import stapl.core._
import stapl.core.pdp.PDP

object EhealthPolicyTest {
  
  import TestExample._
  
  val converter = new TreeConverter(TestPol, Set.empty)
  converter.convertTree();
  val pdp:PDP = new PDP(converter.root)
  
  @BeforeClass def setup() {

  }
}
/**
 * Correctness tests of the e-health policy.
 */
class EhealthPolicyTest extends AssertionsForJUnit {
  
  import EhealthPolicyTest._
  import EhealthPolicy._
  
  @Before def setup() {
    // nothing to do
  }

  @Test def testNotApplicableOtherAction() {
    assert(pdp.evaluate("maarten", "an-action", "doc123").decision === NotApplicable)
  }

  @Test def testDenyWithdrawnConsents() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel"),
        subject.triggered_breaking_glass -> false,
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")).decision 
        === Deny)
  }

  @Test def testDenyIncorrectMedicalPersonnel() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "role-not-allowed"),
        subject.triggered_breaking_glass -> false,
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")).decision === 
         Deny)
  }

  @Test def testPhysicianDepartment1() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "department-not-allowed",
        resource.type_ -> "patientstatus",
        resource.operator_triggered_emergency -> false,
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")).decision === 
          Deny)
  }

  @Test def testPhysicianDepartment2() {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten"))
    assert(result.decision === Permit) // ignore the obligations
  }

  @Test def testPhysicianDepartment3() {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "elder_care",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten"))
    assert(result.decision === Permit) // ignore the obligations
  }

  @Test def testPhysicianDepartment4() {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "emergency",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten"))
    assert(result.decision === Permit) // ignore the obligations
  }

  @Test def testPermitPhysicianEmergency1() {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"))
    assert(result.decision === Permit)
  }

  @Test def testPermitPhysicianEmergency2() {        
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"),
        resource.operator_triggered_emergency -> true)
    assert(result.decision === Permit)
  }

  @Test def testPermitPhysicianEmergency3() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"),
        resource.operator_triggered_emergency -> false,
        resource.indicates_emergency -> true).decision === Permit)
  }

  @Test def testOverrideWithdrawnConsents {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten"))
        .decision === Permit)
  }
  
  @Test def testPermitNurseOfElderCareDepartment {
    assert(pdp.evaluate("maarten", "view", "doc123",
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
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1)).decision === Permit)
  }
  
  @Test def testDenyNurseOfElderCareDepartmentNotAllowed {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> false, // X
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
        assert(result.decision === Deny)
  }
  
  @Test def testDenyNurseOfElderCareDepartmentNotAtHospital {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "somewhere-not-the-hospital", // X
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        assert(result.decision === Deny)
  }
  
  @Test def testDenyNurseOfElderCareDepartmentNotInNurseUnit {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientZ", "patientY"), // X 
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        assert(result.decision === Deny)
  }
  
  @Test def testDenyNurseOfElderCareDepartmentNotResponsible {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientY", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        assert(result.decision === Deny)
  }
  
  @Test def testDenyNurseOfElderCareDepartmentNotOwner {
    val result = pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientA",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        assert(result.decision === Deny)
  }
  
  @Test def testDenyNurseOfElderCareDepartmentTooLongAgo {
    val result = pdp.evaluate("maarten", "view", "doc123",
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
        resource.created -> new LocalDateTime(2014, 6, 1, 14, 2, 1), // X more than five days ago
        environment.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
        assert(result.decision === Deny)
  }
}

object TestExample extends BasicPolicy with GeneralTemplates {
  
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
  val TestPol = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (    
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