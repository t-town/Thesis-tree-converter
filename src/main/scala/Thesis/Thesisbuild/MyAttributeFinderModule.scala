package Thesis.Thesisbuild

import stapl.core.pdp.AttributeFinderModule
import stapl.core.AttributeContainerType
import stapl.core.ConcreteValue
import stapl.core.AttributeType
import stapl.core.pdp.EvaluationCtx
import stapl.core._
import org.joda.time.LocalDateTime

class MyAttributeFinderModule extends AttributeFinderModule {
  
  //Test9
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    Thread.sleep(1000)
    cType match {
      case SUBJECT => name match {
        case "roles" => ctx.subjectId match {
          case "test1" => Some(List("medical_personnel"))
          case "test2"|"test3"|"test4" => Some(List("medical_personnel", "physician"))
          case "test5" => Some(List("medical_personnel", "nurse"))
        }
        case "triggered_breaking_glass" => ctx.subjectId match {
          case "test1"|"test5" => Some(false)
          case "test2"|"test3"|"test4" => Some(true)
        }
        case "department" => ctx.subjectId match {
          case "test2"|"test3"|"test4" => Some("cardiology")
          case "test5" => Some("elder_care")
        }
        case "current_patient_in_consultation" => null //TODO
        case "treated_in_last_six_months" => null //TODO
        case "primary_patients" => null //TODO
        case "is_head_physician" => null //TODO
        case "treated" => null //TODO
        case "treated_by_team" => null //TODO
        case "admitted_patients_in_care_unit" => null //TODO
        case "shift_start" => Some(new LocalDateTime(2014, 6, 24, 9, 0, 0))
        case "shift_stop" => Some(new LocalDateTime(2014, 6, 24, 17, 0, 0))
        case "location" => Some("hospital")
        case "admitted_patients_in_nurse_unit" => Some(List("patientX", "patientY"))
        case "allowed_to_access_pms" => Some(true)
        case "responsible_patients" => Some(List("patientX", "patientZ"))
        case _ => None
      }
      case RESOURCE => name match {
        case "owner:id" => Some("patientX")
        case "owner_withdrawn_consents" => ctx.subjectId match{
          case "test1" => Some(List("subject1","subject2","subject3","test1"))
          case "test2" => Some(List("subject1","subject2","subject3","test2"))
          case "test3" => Some(List("subject1","subject2","subject3"))
          case "test4" => Some(List("subject1","subject2","subject3","test4"))
          case "test5" => Some(List("subject1"))
        }
        case "type_" => ctx.subjectId match {
          case "test1"|"test2"|"test3"|"test4"|"test5" => Some("patientstatus")
        }
        case "created" => Some(new LocalDateTime(2014, 6, 22, 14, 2, 1))
        case "operator_triggered_emergency" => null //TODO
        case "indicates_emergency" => null //TODO
        case "owner_responsible_physicians" => null //TODO
        case "owner_discharged" => null //TODO
        case "owner_discharged_dateTime" => null //TODO
        case "patient_status" => null //TODO
        case _ => None
      }
      case ENVIRONMENT => name match {
        case "currentDateTime" => Some(new LocalDateTime(2014, 6, 24, 14, 2, 1))
        case _ => None
      }
      case _ => None
    }
  }
  

}