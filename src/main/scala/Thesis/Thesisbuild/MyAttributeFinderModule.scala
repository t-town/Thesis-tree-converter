package Thesis.Thesisbuild

import stapl.core.pdp.AttributeFinderModule
import stapl.core.AttributeContainerType
import stapl.core.ConcreteValue
import stapl.core.AttributeType
import stapl.core.pdp.EvaluationCtx
import stapl.core._

class MyAttributeFinderModule extends AttributeFinderModule {
  
  //TODO hardcoden voor tests 
  //TODO include sleep how?
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    cType match {
      case SUBJECT => name match {
        case "roles" => null //TODO
        case "triggered_breaking_glass" => null //TODO
        case "department" => null //TODO
        case "current_patient_in_consultation" => null //TODO
        case "treated_in_last_six_months" => null //TODO
        case "primary_patients" => null //TODO
        case "is_head_physician" => null //TODO
        case "treated" => null //TODO
        case "treated_by_team" => null //TODO
        case "admitted_patients_in_care_unit" => null //TODO
        case "shift_start" => null //TODO
        case "shift_stop" => null //TODO
        case "location" => null //TODO
        case "admitted_patients_in_nurse_unit" => null //TODO
        case "allowed_to_access_pms" => null //TODO
        case "responsible_patients" => null //TODO
        case _ => None
      }
      case RESOURCE => name match {
        case "owner_id" => null //TODO
        case "owner_withdrawn_consents" => null //TODO
        case "type" => null //TODO
        case "created" => null //TODO
        case "operator_triggered_emergency" => null //TODO
        case "indicates_emergency" => null //TODO
        case "owner_responsible_physicians" => null //TODO
        case "owner_discharged" => null //TODO
        case "owner_discharged_dateTime" => null //TODO
        case "patient_status" => null //TODO
        case _ => None
      }
      case ENVIRONMENT => name match {
        case "currentDateTime" => return null //TODO
        case _ => None
      }
      case _ => None
    }
    None 
  }
  

}