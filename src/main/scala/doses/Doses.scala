package doses

import org.joda.time.DateTime
import models.model._

trait AgeRange {
  def withinRange(
    dose: Option[DateTime],
    dob: DateTime,
    startMonth: Int,
    endMonth: Int): Boolean = {
    dose match {
      case Some(d) => d.isAfter(dob.plusMonths(startMonth)) && (d.isBefore(dob.plusMonths(endMonth)))
      case None => false // Might want an exception here.
    }
  }
}

class DoseMap(name: String, doses: DateMap) {
  // Get the nth dose, using ordinal numbers. All the elements of the map are
  // assumed to have the a name in a series, such as "dtap1", "dtap2", ...
  def nth(n: Int): Option[DateTime] =
    if (doses.contains(name + n)) Some(doses(name + n)) else None
  def firstDose: Option[DateTime] = nth(1)
  def secondDose: Option[DateTime] = nth(2)
  def thirdDose: Option[DateTime] = nth(3)
  def fourthDose:Option[DateTime] =  nth(4)
  def fifthDose: Option[DateTime] = nth(5)
}

// Common operations on the doses given.
class Doses(name: String, dob: DateTime, doses: DateMap)
    extends DoseMap(name, doses) with AgeRange {
  def doseNwithinAgePeriod(dose: Int, startMonth: Int, endMonth: Int): Boolean =
    withinRange(nth(dose), dob, startMonth, endMonth)
  def doseIsAfter(dose: DateTime, dob: DateTime, nMonths: Int): Boolean =
    dose.isAfter(dob.plusMonths(nMonths))
  def doseIsBefore(dose: DateTime, dob: DateTime, nMonths: Int): Boolean =
    dose.isBefore(dob.plusMonths(nMonths))
  // Determine if one dose is after another plus a number of days.
  def doseAfterDose(dose1: DateTime, dose2: DateTime, days: Int): Boolean =
    dose2.isAfter(dose1.plusDays(days))
  // Determine if one dose is before another plus a number of days.
  def doseBeforeDose(dose1: DateTime, dose2: DateTime, days: Int): Boolean =
    dose2.isBefore(dose1.plusDays(days))
}
