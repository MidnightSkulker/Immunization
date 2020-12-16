// package scala.language.implicitConversions
// import cats.effect.IO
import org.joda.time.DateTime
import scala.util.matching.Regex

class Vapid {}

trait VaccineStatus {
  // type VaccineStatus = Value
  // val Error, Incomplete, UpToDate, Complete = Value
  type VaccineStatus = Int
  val NA = 0
  val Error = 1
  val Incomplete = 2
  val UpToDate = 3
  val Complete = 4
  // Make a summary status for the immunizations.
  // All must be Complete to get a complete status.
  // Otherwise, the status is the "least" status amongst the vaccines.
  // For example, if there is at least one Error, the whole record is in Error.
  // If there is no Error, but at least one Incomplete, the record is incomplete.
  // If there are no Errors or Incompletes, and there is at least one UpToDate,
  // then the record is UpToDate.
  // Finally, if everything is Complete, the record is Complete.
  def summaryStatus(ss: List[VaccineStatus]): VaccineStatus =
    if (ss.exists(x => x == Error)) Error
    else if (ss.exists(x => x == Incomplete)) Incomplete
    else if (ss.exists(x => x == UpToDate)) UpToDate
    else Complete

  def outStatus(v: VaccineStatus): String =
    v match {
      case 0 => "Error"
      case 1 => "Incomplete"
      case 2 => "UpToDate"
      case 3 => "Complete"
      case x => "Really Error"
    }
}
