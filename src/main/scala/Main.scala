// Can only declare a package for scalac compilation.
// This is ignored in the REPL, it just generates a warning.
// package com.twitter.example
// package scala.language.implicitConversions
// import cats.effect.IO
import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
import scala.io.Source._
import scala.io.BufferedSource
import org.joda.time.base.AbstractInstant
import org.joda.time.DateTime
import org.joda.time.Period
import scala.util.matching.Regex

trait VaccineStatus extends Enumeration () {
  type VaccineStatus = Value
  val Incomplete, UpToDate, Complete, Error = Value
}

trait AgeRange {
  def withinRange(d: DateTime, dob: DateTime, startMonth: Int, endMonth: Int): Boolean =
    (d.isAfter(dob.plusMonths(startMonth)) && (d.isBefore(dob.plusMonths(endMonth))))
  def withinRange(d: Option[DateTime], dob: DateTime, startMonth: Int, endMonth: Int): Boolean =
    d match {
      case None => false
      case Some(d) =>
        (d.isAfter(dob.plusMonths(startMonth)) && (d.isBefore(dob.plusMonths(endMonth))))
    }
}

trait Older {
  def olderThan(dob: DateTime, numberOfMonths: Int): Boolean =
    dob.plusMonths(numberOfMonths).isBefore(DateTime.now())
}

trait Younger {
  def youngerThan(dob: DateTime, numberOfMonths: Int): Boolean =
    dob.plusMonths(numberOfMonths).isAfter(DateTime.now())
}

trait Recently {
  def recently(dose: DateTime, numberOfMonths: Int): Boolean =
    DateTime.now().plusMonths(-numberOfMonths).isBefore(dose)
  def recently(dose: Option[DateTime], numberOfMonths: Int): Boolean =
    dose match {
      case None => false
      case Some(dose) => DateTime.now().plusMonths(-numberOfMonths).isBefore(dose)
    }
}

// Enumeration -> abstract vaccine -> specific doses.

trait NewBorn {
  // Determine if the child is new born.
  def isNewBorn (dob: DateTime): Boolean = dob.plusMonths(2).isAfter(DateTime.now())
}


// Get information about doses from Json data
class ParseJsonDoses (filename: String) {
  // Convert a buffered source of strings to a string
  def flattenSource(iter: BufferedSource): String = {
    var glob: String = ""
    for (line <- iter) { glob += line }
    return glob
  }
  // Read the Json information from a file.
  def readFile(filename: String): BufferedSource = fromFile(filename)
  // Process the Json information into a string.
  def stringify(file: BufferedSource): String = flattenSource(file)
  // Convert json string into JsValue.
  def jsonify(glob: String): spray.json.JsValue = glob.parseJson
  // Convert JsValue into a list of maps, one per student.
  def mapifyJson(json: spray.json.JsValue): List[Map[String, String]] =
    json.convertTo[List[Map[String, String]]]
  // Process all the way from the file to mapified Json.
  def mapify(filename: String): List[Map[String, String]] =
    stringify(readFile(filename)).parseJson.convertTo[List[Map[String, String]]]
}

// Common operations on the doses given.
class Doses(name: String, dob: DateTime, doses: Array[Option[DateTime]]) extends VaccineStatus with NewBorn with AgeRange {
  // Compute the number of doses of the vaccine given.
  val currentDate = new DateTime() // For some reason this give the current time.
  def numberOfDoses(doses: Array[Option[DateTime]]): Int = doses.count(d => !d.isEmpty)
  def doseNwithinAgePeriod(dose: Int, startMonth: Int, endMonth: Int): Boolean =
    withinRange(doses(dose), dob, startMonth, endMonth)
  def doseIsAfter(dose: DateTime, dob: DateTime, nMonths: Int): Boolean =
    dose.isAfter(dob.plusMonths(nMonths))
  def doseIsAfter(dose: Option[DateTime], dob: DateTime, nMonths: Int): Boolean =
    dose match {
      case None => false
      case Some(dose) => dose.isAfter(dob.plusMonths(nMonths))
    }
  def doseIsBefore(dose: DateTime, dob: DateTime, nMonths: Int): Boolean =
    dose.isBefore(dob.plusMonths(nMonths))
  def doseIsBefore(dose: Option[DateTime], dob: DateTime, nMonths: Int): Boolean =
    dose match {
      case None => false
      case Some(dose) => dose.isBefore(dob.plusMonths(nMonths))
    }
  // Determine if one dose is after another plus a number of days.
  def doseAfterDose(dose1: DateTime, dose2: DateTime, days: Int): Boolean =
    dose2.isAfter(dose1.plusDays(days))
  def doseAfterDose(dose1: Option[DateTime], dose2: Option[DateTime], days: Int): Boolean =
    dose1 match {
      case None => false
      case Some(dose1) =>
        dose2 match {
          case None => false
          case Some(dose2) => dose2.isAfter(dose1.plusDays(days))
        }
    }
  // Determine if one dose is before another plus a number of days.
  def doseBeforeDose(dose1: DateTime, dose2: DateTime, days: Int): Boolean =
    dose2.isBefore(dose1.plusDays(days))
  def doseBeforeDose(dose1: Option[DateTime], dose2: Option[DateTime], days: Int): Boolean =
    dose1 match {
      case None => false
      case Some(dose1) =>
        dose2 match {
          case None => false
          case Some(dose2) => dose2.isBefore(dose1.plusDays(days))
        }
    }
  // Fill up the doses array.
  def fillDoses(doses: Array[DateTime], max: Int): Array[DateTime] = {
    for (j <- 0 to max) { doses(j) = null }
    return null
  }
}

// Vaccinations
// Example names: DTAP, with maximum of 5 shots (vaccinations).
//                Polio, with maximum of 5 shots (vaccinations).
abstract class Vaccine(
  name: String,
  dob: DateTime,
  doses: Array[Option[DateTime]],
  max: Int) extends Doses(name: String, dob: DateTime, doses: Array[Option[DateTime]]) {
  def immunizationStatus (): VaccineStatus = Error
}

// Vaccination status rules for DTAP (Diptheria, Tetanus, Pertussis)
// Other abbreviations used are DTP, DTap, DT, Td, Tdap.
class DTAP (dob: DateTime, doses: Array[Option[DateTime]])
    extends Vaccine("DTAP", dob, doses, 5)
    with Younger
    with VaccineStatus
    with Recently
    with NewBorn {
  override def immunizationStatus (): VaccineStatus =
  numberOfDoses(doses) match {
    case 0 =>
      if (isNewBorn(dob)) UpToDate
      else Incomplete
    case 1 =>
      val dose1 = doses(0)
      // Less than 2 months old or child is less than 4 months of age.
      if ((youngerThan(dob, 4)) || (recently(dose1, 2))) UpToDate
      else Incomplete
    case 2 =>
      val dose2 = doses(1)
      // Dose 1 received at or after 1st birthday and dose 2 received less than 12 months ago.
      if ((doseIsAfter(dose2, dob, 12)) && (recently(dose2, 12))) UpToDate
      // Dose 1 received at or after 1st birthday and dose 2 received more than 12 months ago.
      if ((doseIsAfter(dose2, dob, 12)) && (!recently(dose2, 12))) Incomplete
      // Child is 7 years or older and dose 2 received less than 12 months ago.
      if (((DateTime.now).isAfter(dob.plusMonths(84))) && (recently(dose2, 12))) UpToDate
      // Child is 7 years or older and dose 2 received more than 12 months ago.
      if ((DateTime.now).isAfter(dob.plusMonths(84)) && (!recently(dose2, 12))) Incomplete
      // Dose 2 received less than 2 months ago or child is less than 6 months old.
      if ((recently(dose2, 2)) || (youngerThan(dob, 6))) UpToDate
      // Dose 2 received more than 2 months ago and child is less than 6 months old.
      if ((!recently(dose2, 2)) && (youngerThan(dob, 6))) UpToDate
      else Incomplete
    case 3 =>
      val dose1 = doses(0)
      val dose3 = doses(2)
      // Dose received after 7th birthday.
      if (doseIsAfter(dose3, dob, 84)) Complete
      // Dose 3 received less than 12 months ago.
      if (recently(dose3, 12)) UpToDate
      // Dose 1 received at or after 1st birthday and child is less than 4 years old.
      if (doseIsAfter(dose1, dob, 12) && youngerThan(dob, 48)) UpToDate
      // Child is less than 18 months old.
      if (youngerThan(dob, 18)) UpToDate
      // Dose 3 recceived 12 months or more ago and child is 18 months old or older.
      else Incomplete
    case 4 =>
      val dose4 = doses(3)
      // Dose 4 was given at or after 4th Birthday
      if (doseIsAfter(dose4, dob, 48)) Complete
      // Dose 4 was before 4th Birthday and child is less than Kindergarten age (5).
      if (doseIsBefore(dose4, dob, 48) && youngerThan(dob, 60)) UpToDate
      // Dose 4 was received before 4th birthday and child is kindergarten of higher grade.
      else Incomplete
    case 5 =>
      Complete
    case default => Error
  }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class HIB (dob: DateTime, doses: Array[Option[DateTime]])
    extends Doses("DTAP", dob, doses)
    with Older
    with Younger
    with VaccineStatus
    with NewBorn
    with Recently {
  def immunizationStatus (): VaccineStatus =
    numberOfDoses(doses) match {
      case 0 =>
        // If child is less than 2 months old, HIB is not needed yet.
        if (isNewBorn(dob)) UpToDate
        // If child is more than 4 years old, it is not required.
        if (olderThan(dob, 48)) Complete
        else Incomplete
      case 1 =>
        val dose1 = doses(0)
        // Received less than 2 months ago or child is less than four months old.
        if (recently(dose1, 2) && youngerThan(dob, 4)) UpToDate
        // Received after 15 months old, age less than 18 months.
        // Received after 15 months old, age less than 60 months.
        if (doseIsAfter(dose1, dob, 15) && youngerThan(dob, 60)) Complete
        if (recently(dose1, 2) && doseIsBefore(dose1, dob, 15)) UpToDate
        // Received before 15 months old, age less than 60 months.
        if (doseIsBefore(dose1, dob, 15) && olderThan(dob, 18) && youngerThan(dob, 60))
          Incomplete
        // Not required for 5 years and older.
        if (olderThan(dob, 60)) Complete
        else Incomplete
      case 2 =>
        val dose1 = doses(0)
        val dose2 = doses(1)
        if (youngerThan(dob, 12)) UpToDate
        // Received second dose after 15 months, age < 18 months.
        if (doseIsAfter(dose2, dob, 15) && youngerThan(dob, 60)) Complete
        // First dose after 12 months, age < 18 months.
        if (doseIsAfter(dose1, dob, 12) && youngerThan(dob, 60)) Complete
        // First dose before 12 months, second dose before 15 moontsh, age < 60 months.
        if (doseIsBefore(dose1, dob, 12) &&
            doseIsBefore(dose2, dob, 15) &&
            youngerThan(dob, 60)) Incomplete
        // Not required for 5 years and older.
        if (olderThan(dob, 60)) Complete
        else Incomplete
      case 3 =>
        val dose3 = doses(2)
        if (youngerThan(dob, 12)) UpToDate
        if (youngerThan(dob, 18) && doseIsAfter(dose3, dob, 12)) Complete
        if (youngerThan(dob, 18) && doseIsBefore(dose3, dob, 12)) UpToDate
        if (youngerThan(dob, 60) && doseIsAfter(dose3, dob, 12)) Complete
        if (youngerThan(dob, 60) && doseIsBefore(dose3, dob, 12)) UpToDate
        if (olderThan(dob,60)) Complete
        else Incomplete
      case 4 => Complete
      case default => Error

  }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class Polio (dob: DateTime, doses: Array[Option[DateTime]])
    extends Doses("Polio", dob, doses)
    with Younger
    with VaccineStatus
    with Recently
    with Older {
  def immunizationStatus (): VaccineStatus =
    numberOfDoses(doses) match {
      case 0 =>
        // If child is less than 2 months old, Polio vaccine is not needed yet.
        if (youngerThan(dob, 2)) UpToDate
        // If child is more than 18 years old, Polio vaccine is not needed yet.
        if (olderThan(dob, 18 * 12)) UpToDate
        else Incomplete
      case 1 =>
        val dose1 = doses(0)
        // Dose 1 received less than 2 months ago or child is less than 4 months old.
        if (recently(dose1, 2) || youngerThan(dob, 4)) UpToDate
        // Dose 1 received more than 2 months ago and child is less than 4 months old.
        if (youngerThan(dob, 18 * 12)) Incomplete
        else Complete
      case 2 =>
        val dose2 = doses(1)
        // Dose 2 received less than 12 months ago.
        if (recently(dose2, 12)) UpToDate
        // Child is less that 18 months old
        if (youngerThan(dob, 18)) UpToDate
        else Incomplete
      case 3 =>
        val dose3 = doses(2)
        // Dose 3 received after 4 years
        if (doseIsAfter(dose3, dob, 4 * 12)) Complete
        // Dose 3 received less that 12 months ago
        if (recently(dose3, 12)) UpToDate
        // Child is less than kindergarten age (5)
        if (youngerThan(dob, 5 * 12)) UpToDate
        // Dose 3 received more than 12 months ago, and child is kindergarten or
        // higher age, but less than 18 years old.
        else Incomplete
      case 4 => Complete
    }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class Varicella (
  dob: DateTime,
  diseaseHistory: List[String],
  doses: Array[Option[DateTime]])
    extends Doses("DTAP", dob, doses)
    with Younger
    with Older
    with VaccineStatus
    with Recently {
  def immunizationStatus (): VaccineStatus =
    if (diseaseHistory contains "Chicken Pox") Complete
    else
      numberOfDoses(doses) match {
        case 0 =>
          // No received and child is less than 18 months old.
          if (youngerThan(dob, 18)) UpToDate
          else Incomplete
        case 1 =>
          val dose1 = doses(0)
          // First dose is after age 12 months
          if (doseIsAfter(dose1, dob, 12)) Complete
          // Received before 12 months of age and child is less than 18 months old.
          if (doseIsBefore(dose1, dob, 12) && youngerThan(dob, 18)) UpToDate
          // Received before 12 months of age and child is more than 18 months old.
          if (doseIsBefore(dose1, dob, 12) && olderThan(dob, 18)) Incomplete
          // Received at or after 13 years of age and less than two months ago.
          if (olderThan(dob, 13 * 12) && recently(dose1, 2)) UpToDate
          // Received at or after 13 years of age and less than two months ago.
          if (olderThan(dob, 13 * 12) && !recently(dose1, 2)) Incomplete
          else Incomplete
        case 2 =>
          val dose1 = doses(0)
          val dose2 = doses(1)
          // At least one dose given between 12 months of age and 12 years of age
          if (doseIsAfter(dose1, dob, 12) && doseIsBefore(dose1, dob, 12 * 12)) Complete
          if (doseIsAfter(dose2, dob, 12) && doseIsBefore(dose2, dob, 12 * 12)) Complete
          // First dose given after age 13 and second dose given more than 24
          // days after the first dose.
          if (doseIsAfter(dose1, dob, 13 * 12) && doseAfterDose(dose1, dose2, 24)) Complete
          // Second dose given fewere than 24 days after the first dose and less
          // than two months ago.
          if (doseBeforeDose(dose1, dose2, 24) && recently(dose2, 2)) UpToDate
          // Second dose prior to 12 months of age
          if (doseIsBefore(dose2, dob, 12) && olderThan(dob, 12)) Incomplete
          else Incomplete
        case 3 => Complete
        case default => Error
      }
}

// Vaccination status rules for MMR (Measles, Mumps, and Rubella)
class MMR (
  dob: DateTime,
  doses: Array[Option[DateTime]])
    extends Doses("DTAP", dob, doses)
    with VaccineStatus
    with Younger
    with Older
    with Recently {
  def immunizationStatus (): VaccineStatus =
    if (youngerThan(dob, 5 * 12))
      numberOfDoses(doses) match {
        case 0 =>
          // Child is under 15 months old
          if (youngerThan(dob, 15)) UpToDate
          else Incomplete
        case 1 =>
          // **** Need to restrict this to less than K age.
          val dose1 = doses(0)
          // Received after 12 months of age.
          if (doseIsAfter(dose1, dob, 12)) Complete
          // Received prior to 12 months of age and child is under 15 months of age.
          if (doseIsBefore(dose1, dob, 12) && youngerThan(dob, 15)) UpToDate
          else Incomplete
        case default => Complete
      } // match
    else
      numberOfDoses(doses) match {
        case 0 => Incomplete
        case 1 =>
          val dose1 = doses(0)
          // Received less that two months ago.
          if (recently(dose1, 2)) UpToDate
          else Incomplete
        case 2 =>
          val dose1 = doses(0)
          val dose2 = doses(1)
          // Received after 12 months of age.
          if (doseIsAfter(dose1, dob, 12) && doseAfterDose(dose1, dose2, 24)) Complete
          // First dose received prior to 12 months of age and second
          // received less than two months ago.
          if (doseIsBefore(dose1, dob, 12) && recently(dose2, 2)) UpToDate
          // First does received prior to 12 months of age and second
          // dose received two or more months ago.
          if (doseIsBefore(dose1, dob, 12) && !recently(dose2, 2)) Incomplete
          // Second dose given fewer than 24 days after the first
          // dose and less than two months ago.
          if (!doseAfterDose(dose1, dose2, 24) && !recently(dose2, 2)) UpToDate
          // Second dose given fewer than 24 days after first dose and two or more months ago.
          if (!doseAfterDose(dose1, dose2, 24) && recently(dose2, 2)) Incomplete
          else Incomplete
        case 3 => Complete
      } // match
}

// Vaccination status rules for HEPA (Hepatitis A)
class HEPA (
  dob: DateTime,
  doses: Array[Option[DateTime]])
    extends Doses("DTAP", dob, doses)
    with VaccineStatus
    with Younger
    with Recently {
  def immunizationStatus (): VaccineStatus =
    numberOfDoses(doses) match {
      case 0 =>
        // Child is under 18 months old
        if (youngerThan(dob, 18)) UpToDate
        else Incomplete
      case 1 =>
        val dose1 = doses(0)
        // Received after 12 months of age and less than 12 months ago.
        if (doseIsAfter(dose1, dob, 12) && recently(dose1, 12)) UpToDate
        // Received after 12 months of age and more that 12 months ago.
        if (doseIsAfter(dose1, dob, 12) && !recently(dose1, 12)) Incomplete
        // Received prior to 12 months of age and child is less that 18 months of age.
        if (doseIsBefore(dose1, dob, 12) && youngerThan(dob, 18)) UpToDate
        // Recieved prior to 12 months of age and child is 18 months or older.
        else Incomplete
      case 2 =>
        val dose1 = doses(0)
        val dose2 = doses(1)
        // Both doses received after 12 months of age.
        if (doseIsAfter(dose1, dob, 12) && doseIsAfter(dose2, dob, 12)) Complete
        // Dose 1 received prior to 12 months of age and dose 2
        // received less than 2 months ago.
        if (doseIsBefore(dose1, dob, 12) && recently(dose2, 12)) UpToDate
        // Dose 1 received priot to 12 months of age and dose 2
        // received 12 months of more ago.
        else Incomplete
      case 3 => Complete
      case 4 => Complete
  }
}

// Vaccination status rules for HEPB (Hepatitis B)
class HEPB (
  dob: DateTime,
  doses: Array[Option[DateTime]])
    extends Doses("DTAP", dob, doses)
    with VaccineStatus
    with Younger
    with Older
    with Recently {
  def immunizationStatus (): VaccineStatus =
    numberOfDoses(doses) match {
      case 0 =>
        // Child is under 2 months old.
        if (youngerThan(dob, 2)) UpToDate
        // Child is 2 months old or older.
        else Incomplete
      case 1 =>
        val dose1 = doses(0)
        // Received at or after 11 years old and less than 6 months ago.
        if (olderThan(dob, 11 * 12) && recently(dose1, 6)) UpToDate
        // Received at or after 11 years of age and 6 months or more ago.
        if (olderThan(dob, 11 * 12) && !recently(dose1, 6)) Incomplete
        // Received before 11 years of age and less than 2 months ago or
        // child is less than 4 months old.
        if ((doseIsBefore(dose1, dob, 11 * 12) &&
             recently(dose1, 2)) || youngerThan(dob, 4)) UpToDate
        // Received before 11 years of age and more than 2 months ago
        // and child is 4 months old or older.
        if ((doseIsBefore(dose1, dob, 11 * 12) &&
            !recently(dose1, 2)) &&
            olderThan(dob, 4)) Incomplete
        else Incomplete
      case 2 =>
        val dose1 = doses(0)
        val dose2 = doses(1)
        // First dose received at or after 11 years old and second dose
        // received at least 4 months after first does.
        if (doseIsBefore(dose1, dob, 11 * 12) && doseAfterDose(dose1, dose2, 4)) Complete
        // First dose is received at or after 18 years old.
        if (doseIsAfter(dose1, dob, 18 * 12)) Complete
        // Second dose received less that 5 months ago
        if (recently(dose2, 5)) UpToDate
        // Received 5 months or more ago and child is less than 18 months old.
        if (!recently(dose2, 5) && youngerThan(dob, 18)) UpToDate
        // Received 5 months or more ago and child is more than 18 months old.
        else Incomplete
      case 3 => Complete
    }
}

class Student(jsonMap: Map[String, String]) {
  def filterShots(jm: Map[String, String], shotRegex: String): List[String] =
    jm.filterKeys(_.matches(shotRegex)).values.toList
  val dob = jsonMap("dob")
  val fullName = jsonMap("firstName") + " " + jsonMap("lastName")
  val dtapShots = filterShots(jsonMap, "dtap.*")
  val polioShots = filterShots(jsonMap, "polio.*")
  val mmrShots = filterShots(jsonMap, "mmr.*")
  val varicellaShots = filterShots(jsonMap, "varicella.*")
  val hibShots = filterShots(jsonMap, "hib.*")
  val hepaShots = filterShots(jsonMap, "hepa.*")
  val hepbShots = filterShots(jsonMap, "hepb.*")
}

// ----------------------------------------------------------------------------
object ImmunizationReport {
  def main(args: Array[String]): Unit = {

    // Object to parse the JSON information.
    val parser = new ParseJsonDoses ("inputs/ImmunizationData.json")
    val doseMaps: List[Map[String, String]] = parser.mapify("inputs/ImmunizationData.json")
    val x1 = doseMaps(0)("dtap1")
    val x2 = doseMaps(1)("dtap2")
    println("======= > x1, x2 = ", x1, x2)

    val sample2: String = """[{
  "dob": "2011-08-08T00:00:00.000Z",
  "dtap1": "2011-10-27T00:00:00.000Z",
  "dtap2": "2011-12-13T00:00:00.000Z",
  "dtap3": "2012-02-11T00:00:00.000Z",
  "dtap4": "2012-10-09T00:00:00.000Z",
  "firstName": "Yuvan",
  "hepA1": "2012-08-09T00:00:00.000Z",
  "hepA2": "2013-02-06T00:00:00.000Z",
  "hepB1": "2011-08-08T00:00:00.000Z",
  "hepB2": "2011-10-27T00:00:00.000Z",
  "hepB3": "2012-02-11T00:00:00.000Z",
  "hib1": "2011-10-27T00:00:00.000Z",
  "hib2": "2011-12-13T00:00:00.000Z",
  "hib3": "2012-02-11T00:00:00.000Z",
  "hib4": "2012-10-09T00:00:00.000Z",
  "lastName": "Huppala",
  "mmr1": "2012-08-09T00:00:00.000Z",
  "polio1": "2011-10-27T00:00:00.000Z",
  "polio2": "2011-12-13T00:00:00.000Z",
  "polio3": "2012-02-11T00:00:00.000Z",
  "varicella1": "2012-08-09T00:00:00.000Z"
},
{
  "firstName": "Aaditi",
  "lastName": "Baddrireddi",
  "dob": "2010-03-31T00:00:00.000Z",
  "dtap1": "2010-06-23T00:00:00.000Z",
  "dtap2": "2010-08-06T00:00:00.000Z",
  "dtap3": "2010-11-05T00:00:00.000Z",
  "dtap4": "2011-10-03T00:00:00.000Z",
  "polio1": "2010-06-23T00:00:00.000Z",
  "polio2": "2010-08-06T00:00:00.000Z",
  "polio3": "2010-11-05T00:00:00.000Z",
  "varicella1": "2011-04-25T00:00:00.000Z",
  "mmr1": "2011-04-25T00:00:00.000Z",
  "hepB1": "2010-04-02T00:00:00.000Z",
  "hepB2": "2010-06-23T00:00:00.000Z",
  "hepB3": "2010-08-06T00:00:00.000Z",
  "hepB4": "2010-11-05T00:00:00.000Z",
  "hepA1": "2011-04-25T00:00:00.000Z",
  "hepA2": "2011-10-24T00:00:00.000Z",
  "hib1": "2010-08-06T00:00:00.000Z",
  "hib2": "2010-11-05T00:00:00.000Z",
  "hib3": "2011-10-24T00:00:00.000Z",
  "hib4": "2010-06-23T00:00:00.000Z"
}]
"""

    val sample2Ast: spray.json.JsValue = sample2.parseJson
    val sample2Json: String = sample2Ast.prettyPrint
    println("\n------ sample2Json > " + sample2Json)
    val sample2Map: List[Map[String, String]] = sample2Ast.convertTo[List[Map[String, String]]]
    println("\n------ sample2Map > " + sample2Map)
    val student1: Map[String, String] = sample2Map(0)
    println("\n------ student1 > " + student1)

    // println("\n------ 2000 characters from > " + filename)
    // print(glob.take(2000))
    // println("\n------ parsedJson > " + jsonImmunizations.prettyPrint)
    // println("\n------ jsonListMap > " + jsonImmunizations.prettyPrint)
    // val jsonListMap = jsonAst.convertTo[List[Map[String, String]] ]
    // println("\n------ characters straight from >>>> " + filename)
    // var j = 0
    // for (line <- file.getLines) {
    //   if (j < 12) {
    //     println(line)
    //     j += 1
    //   }
    // }
  }
}

// List<String> SCOPES_ARRAY = Arrays.asList(
//         "https://www.googleapis.com/auth/drive.file",
//         "https://www.googleapis.com/auth/userinfo.email",
//         "https://www.googleapis.com/auth/userinfo.profile",
//         "https://docs.google.com/feeds",
//         "https://spreadsheets.google.com/feeds");
// http://blog.pamelafox.org/2013/06/exporting-google-spreadsheet-as-json.html
