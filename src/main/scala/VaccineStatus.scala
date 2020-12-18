// package scala.language.implicitConversions
// import cats.effect.IO
import org.joda.time.DateTime
import scala.util.matching.Regex

// class Vapid {}

trait VaccineStatuses {
  type VaccineStatuses = Int
  val NA: VaccineStatuses = 0
  val Error: VaccineStatuses = 1
  val Incomplete: VaccineStatuses = 2
  val UpToDate: VaccineStatuses = 3
  val Complete: VaccineStatuses = 4
}

trait VaccineStatus extends VaccineStatuses {
  // type VaccineStatus = Value
  // val Error, Incomplete, UpToDate, Complete = Value
  // Make a summary status for the immunizations.
  // All must be Complete to get a complete status.
  // Otherwise, the status is the "least" status amongst the vaccines.
  // For example, if there is at least one Error, the whole record is in Error.
  // If there is no Error, but at least one Incomplete, the record is incomplete.
  // If there are no Errors or Incompletes, and there is at least one UpToDate,
  // then the record is UpToDate.
  // Finally, if everything is Complete, the record is Complete.
  def summaryStatus(ss: List[VaccineStatuses]): VaccineStatuses =
    if (ss.exists(x => x == Error)) Error
    else if (ss.exists(x => x == Incomplete)) Incomplete
    else if (ss.exists(x => x == UpToDate)) UpToDate
    else Complete

  def outStatus(v: VaccineStatuses): String =
    v match {
      case 0 => "NA"
      case 1 => "Error"
      case 2 => "Incomplete"
      case 3 => "UpToDate"
      case 4 => "Complete"
      case x => "Really Error"
    }
}
