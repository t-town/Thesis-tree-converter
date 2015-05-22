package Thesis.Thesisbuild

import stapl.core.pdp.AttributeFinderModule
import stapl.core.AttributeContainerType
import stapl.core.ConcreteValue
import stapl.core.AttributeType
import stapl.core.pdp.EvaluationCtx
import stapl.core._
import org.joda.time.LocalDateTime

class MyAttributeFinderModule extends AttributeFinderModule {
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    Thread.sleep(2)
    cType match {
      case SUBJECT => name match {
        case "roles" => ctx.subjectId match {
          case "test1" => Some(List("medical_personnel"))
          case "test2"|"test3"|"test4"|"test8"|"test10" => Some(List("medical_personnel", "physician"))
          case "test5"|"test6"|"test7"|"test9" => Some(List("medical_personnel", "nurse"))
        }
        case "triggered_breaking_glass" => ctx.subjectId match {
          case "test1"|"test5"|"test6"|"test7"|"test9" => Some(false)
          case "test2"|"test3"|"test4"|"test8"|"test10" => Some(true)
        }
        case "department" => ctx.subjectId match {
          case "test2"|"test3"|"test4"|"test10" => Some("cardiology")
          case "test5"|"test6"|"test7"|"test8"|"test9" => Some("elder_care")
        }
        case "current_patient_in_consultation" => null
        case "treated_in_last_six_months" => null 
        case "primary_patients" => null 
        case "is_head_physician" => null
        case "treated" => null 
        case "treated_by_team" => null 
        case "admitted_patients_in_care_unit" => null 
        case "shift_start" => Some(new LocalDateTime(2014, 6, 24, 9, 0, 0))
        case "shift_stop" => Some(new LocalDateTime(2014, 6, 24, 17, 0, 0))
        case "location" => ctx.subjectId match {
          case "test6" =>  Some("somewhere-not-the-hospital")
          case _ => Some("hospital")
        }
        case "admitted_patients_in_nurse_unit" => Some(List("patientX", "patientY"))
        case "allowed_to_access_pms" => Some(true)
        case "responsible_patients" => Some(List("patientX", "patientZ"))
        case _ => None
      }
      case RESOURCE => name match {
        case "owner:id" => Some("patientX")
        case "owner_withdrawn_consents" => ctx.subjectId match{
          case "test3" => Some(List("subject1","subject2","subject3"))
          case "test5"|"test6"|"test7"|"test9" => Some(List("subject1"))
          case x => Some(List("subject1","subject2","subject3",x))
        }
        case "type_" =>  Some("patientstatus")
        case "created" => Some(new LocalDateTime(2014, 6, 22, 14, 2, 1))
        case "operator_triggered_emergency" => Some(false)
        case "indicates_emergency" => Some(false)
        case "owner_responsible_physicians" => null
        case "owner_discharged" => null
        case "owner_discharged_dateTime" => null
        case "patient_status" => null
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

class FinderModule2 extends AttributeFinderModule {
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    Thread.sleep(2)
    cType match {
      case SUBJECT => name match {
        case "roles" => ctx.subjectId match {
          case "test1"|"test2"|"test3"|"test4"|"test5" => Some(List("nurse"))
          case _ => Some(List("physician"))
        }
        case "triggered_breaking_glass" => null
        case "department" => ctx.subjectId match {
          case "test2" => Some("emergency")
          case "test1"|"test5" => Some("psychiatry")
          case _ => Some("radiology")
        }
        case "current_patient_in_consultation" => null
        case "treated_in_last_six_months" => null
        case "primary_patients" => ctx.subjectId match {
          case "test4" => Some(List("jos","owner"))
          case _ => Some(List("owner"))
        }
        case "is_head_physician" => ctx.subjectId match{
          case "test6" => Some(true)
          case _ => Some(false)
        }
        case "treated" => null
        case "treated_by_team" => null
        case "admitted_patients_in_care_unit" => null
        case "shift_start" => Some(new LocalDateTime(2015, 6, 21, 9, 0, 0))
        case "shift_stop" => ctx.subjectId match{
          case "test3" => Some(new LocalDateTime(2015, 6, 21, 17, 0, 0))
          case _ => Some(new LocalDateTime(2015, 6, 21, 12, 0, 0))
        }
        case "location" => ctx.subjectId match {
          case "test1" =>  Some("external")
          case _ => Some("hospital")
        }
        case "admitted_patients_in_nurse_unit" => Some(List("patientX", "patientY"))
        case "allowed_to_access_pms" => ctx.subjectId match {
          case "test7" => Some(true)
          case _ => Some(false)
        }
        case "responsible_patients" => Some(List("patientX", "patientZ"))
        case _ => None
      }
      case RESOURCE => name match {
        case "owner:id" => Some("jos")
        case "owner_withdrawn_consents" => ctx.subjectId match{
          case "test8" => Some(List("test8","owner"))
          case "test9" => Some(List("test9","owner"))
          case _ => Some(List("owner"))
        }
        case "type_" =>  Some("patientstatus")
        case "created" => null
        case "operator_triggered_emergency" => Some(false)
        case "indicates_emergency" => ctx.subjectId match {
          case "test8" => Some(true)
          case _ => Some(false)
        }
        case "owner_responsible_physicians" => null
        case "owner_discharged" => null
        case "owner_discharged_dateTime" => null
        case "patient_status" => ctx.subjectId match {
          case "test2" => Some("deceased")
          case _ => Some("in treatment")
        }
        case _ => None
      }
      case ENVIRONMENT => name match {
        case "currentDateTime" => Some(new LocalDateTime(2015, 6, 21, 13, 0, 0))
        case _ => None
      }
      case _ => None
    }
  }
}

class FinderModule3 extends AttributeFinderModule {
  
  override protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
    Thread.sleep(2)
    return null //TODO
  }
}
