package Thesis.Thesisbuild

import stapl.templates.general.GeneralTemplates
import stapl.core._
import org.joda.time.LocalDateTime

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

object LittleReuse extends BasicPolicy with GeneralTemplates {  
  
  import stapl.core.dsl._
  
  environment.currentDateTime = SimpleAttribute(DateTime)
  resource.type_ = SimpleAttribute(String) 
  resource.owner_withdrawn_consents = ListAttribute(String)
  resource.indicates_emergency = SimpleAttribute(Bool)
  resource.owner_id = SimpleAttribute("owner:id", String)
  subject.roles = ListAttribute(String)
  subject.department = SimpleAttribute(String)
  subject.primary_patients = ListAttribute(String)
  subject.is_head_physician = SimpleAttribute(Bool)
  subject.shift_start = SimpleAttribute(DateTime)
  subject.shift_stop = SimpleAttribute(DateTime)
  subject.location = SimpleAttribute(String) 
  subject.allowed_to_access_pms = SimpleAttribute(Bool)
  
  val tree2 = Policy("littleReuse") := when(action.id === "view" & resource.type_ === "patientstatus") apply FirstApplicable to (
     Policy("policy:1") := when("nurse" in subject.roles) apply PermitOverrides to (
       Rule("rule:11") := deny iff (!(subject.location === "hospital")),
       Rule("rule:12") := permit iff (subject.department === "emergency"),
       Policy("policy:11") := when(subject.department === "radiology") apply FirstApplicable to (
           Rule("rule:111") := permit iff ((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)),
           Rule("rule:112") := permit iff (resource.owner_id in subject.primary_patients),
           Rule("rule:113") := deny
       ),
       Rule("rule:13") := deny
     ),
     Policy("policy:2") := when("physician" in subject.roles) apply PermitOverrides to (
         Rule("rule:21") := permit iff (subject.is_head_physician),
         Rule("rule:22") := permit iff (subject.allowed_to_access_pms),
         Policy("policy:21") := when(subject.id in resource.owner_withdrawn_consents) apply PermitOverrides to (
           Rule("rule:211") := permit iff (resource.indicates_emergency),
           Rule("rule:212") := deny
         ),
         Rule("rule:23") := deny
     ),
     Rule("rule:3") := deny
  )
  
  val tree2Copy = Policy("littleReuse") := when(action.id === "view" & resource.type_ === "patientstatus") apply FirstApplicable to (
     Policy("policy:1") := when("nurse" in subject.roles) apply PermitOverrides to (
       Rule("rule:11") := deny iff (!(subject.location === "hospital")),
       Rule("rule:12") := permit iff (subject.department === "emergency"),
       Policy("policy:11") := when(subject.department === "radiology") apply FirstApplicable to (
           Rule("rule:111") := permit iff ((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)),
           Rule("rule:112") := permit iff (resource.owner_id in subject.primary_patients),
           Rule("rule:113") := deny
       ),
       Rule("rule:13") := deny
     ),
     Policy("policy:2") := when("physician" in subject.roles) apply PermitOverrides to (
         Rule("rule:21") := permit iff (subject.is_head_physician),
         Rule("rule:22") := permit iff (subject.allowed_to_access_pms),
         Policy("policy:21") := when(subject.id in resource.owner_withdrawn_consents) apply PermitOverrides to (
           Rule("rule:211") := permit iff (resource.indicates_emergency),
           Rule("rule:212") := deny
         ),
         Rule("rule:23") := deny
     ),
     Rule("rule:3") := deny
  )
}

object suchReuse extends BasicPolicy with GeneralTemplates {

  import stapl.core.dsl._
  
  environment.currentDateTime = SimpleAttribute(DateTime)
  resource.owner_id = SimpleAttribute("owner:id", String)
  resource.created = SimpleAttribute(DateTime)
  resource.type_ = SimpleAttribute(String)
  resource.patient_status = SimpleAttribute(String)
  resource.indicates_emergency = SimpleAttribute(Bool)
  resource.owner_discharged = SimpleAttribute("owner:discharged", Bool)
  resource.owner_discharged_dateTime = SimpleAttribute("owner:discharged_dateTime", DateTime)
  subject.roles = ListAttribute(String)
  subject.location = SimpleAttribute(String)
  subject.shift_start = SimpleAttribute(DateTime)
  subject.shift_stop = SimpleAttribute(DateTime)
  subject.responsible_patients = ListAttribute(String)
  subject.is_head_physician = SimpleAttribute(Bool)
  subject.treated = ListAttribute(String)
  subject.current_patient_in_consultation = SimpleAttribute(String)
  subject.treated_in_last_six_months = ListAttribute(String)
  
  val tree3= Policy("Policy1") := when (action.id === "view" & resource.type_ === "patientstatus") apply FirstApplicable to (
      Policy("Policy2") := when ("nurse" in subject.roles) apply PermitOverrides to (
        Rule("Rule21") := permit iff ((subject.location === "hospital")|(subject.location === "home")),
        Policy("Policy21") := when(resource.created === environment.currentDateTime) apply PermitOverrides to ( 
          Rule("Rule211") := permit iff !(resource.patient_status === "in treatment"),
          Rule("Rule212") := permit iff ((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)) ,
          Policy("Policy211") := when ((resource.indicates_emergency)&(subject.location === "hospital")) apply PermitOverrides to ( 
            Rule("Rule2111") := permit iff (resource.owner_id in subject.responsible_patients ),
            Rule("Rule2112") := deny
          ),
          Rule("Rule213") := deny
        ),
        Rule("Rule22") := deny
      ),
      Policy("Policy3") := when ((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)) apply DenyOverrides to (
        Rule("Rule31") := deny iff resource.owner_discharged,
        Policy("Policy31") := when (subject.location === "hospital") apply FirstApplicable to (
          Policy("Policy311") := when (!(resource.patient_status === "in treatment"))apply PermitOverrides to(
            Rule("Rule3111") := permit iff ("physician" in subject.roles),    
            Rule("Rule3112") := permit iff ("nurse" in subject.roles),  
            Rule("Rule3113") := deny
          ),
          Rule("Rule311") := deny iff (resource.owner_discharged_dateTime gteq environment.currentDateTime - 5.days ),
          Policy("Policy312") := when (subject.is_head_physician) apply PermitOverrides to (
            Rule("Rule3121") := permit iff (resource.indicates_emergency),    
            Rule("Rule3122") := permit iff (("nurse" in subject.roles)|(resource.created === environment.currentDateTime)),  
            Rule("Rule3123") := deny    
          ),
          Rule("Rule312") := deny
        ),
        Rule("Rule32") := permit
      ),
      Rule("Rule1") := deny
  )
  
   val tree3copy= Policy("Policy1") := when (action.id === "view" & resource.type_ === "patientstatus") apply FirstApplicable to (
      Policy("Policy2") := when ("nurse" in subject.roles) apply PermitOverrides to (
        Rule("Rule21") := permit iff ((subject.location === "hospital")|(subject.location === "home")),
        Policy("Policy21") := when(resource.created === environment.currentDateTime) apply PermitOverrides to ( 
          Rule("Rule211") := permit iff !(resource.patient_status === "in treatment"),
          Rule("Rule212") := permit iff ((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)) ,
          Policy("Policy211") := when ((resource.indicates_emergency)&(subject.location === "hospital")) apply PermitOverrides to ( 
            Rule("Rule2111") := permit iff (resource.owner_id in subject.responsible_patients ),
            Rule("Rule2112") := deny
          ),
          Rule("Rule213") := deny
        ),
        Rule("Rule22") := deny
      ),
      Policy("Policy3") := when ((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)) apply DenyOverrides to (
        Rule("Rule31") := deny iff resource.owner_discharged,
        Policy("Policy31") := when (subject.location === "hospital") apply FirstApplicable to (
          Policy("Policy311") := when (!(resource.patient_status === "in treatment"))apply PermitOverrides to(
            Rule("Rule3111") := permit iff ("physician" in subject.roles),    
            Rule("Rule3112") := permit iff ("nurse" in subject.roles),  
            Rule("Rule3113") := deny
          ),
          Rule("Rule311") := deny iff (resource.owner_discharged_dateTime gteq environment.currentDateTime - 5.days ),
          Policy("Policy312") := when (subject.is_head_physician) apply PermitOverrides to (
            Rule("Rule3121") := permit iff (resource.indicates_emergency),    
            Rule("Rule3122") := permit iff (("nurse" in subject.roles)|(resource.created === environment.currentDateTime)),  
            Rule("Rule3123") := deny    
          ),
          Rule("Rule312") := deny
        ),
        Rule("Rule32") := permit
      ),
      Rule("Rule1") := deny
  )
  
}