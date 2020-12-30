// import cats.effect.IO
import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
import scala.io.Source._
import scala.io.BufferedSource
import org.joda.time.base.AbstractInstant
import org.joda.time.DateTime
import java.text.SimpleDateFormat
import org.joda.time.Period
import scala.util.matching.Regex
import models.model._
import json._
import rules._
import doses._

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
}

trait NewBorn {
  // Determine if the child is newly born.
  def isNewBorn (dob: DateTime): Boolean = dob.plusMonths(2).isAfter(DateTime.now())
}

// Vaccinations
// Example names: DTAP, with maximum of 5 shots (vaccinations).
//                Polio, with maximum of 5 shots (vaccinations).
abstract class Vaccine(name: String,
  dob: DateTime,
  doses: DateMap,
  max: Int)
    extends Doses(name: String, dob: DateTime, doses: DateMap) {
  def immunizationStatus (): VaccineStatuses = NA
}

// Vaccination status rules for DTAP (Diptheria, Tetanus, Pertussis)
// Other abbreviations used are DTP, DTap, DT, Td, Tdap.
class DTAP (name: String, dob: DateTime, doses: DateMap)
    extends Vaccine("dtap", dob, doses, 5)
    with Younger
    with Recently
    with NewBorn
    with SpecificRules {
  val rule01: Rule = newBornRule(dob, UpToDate)
  val rule02: Rule = !newBornRule(dob) && doseCountRule(0, Incomplete)
  // One dose, first dose less than 2 months old or child is less than 4 months of age.
  val rule11 = doseCountRule(1) && youngerThanRule(dob, 4) && recentlyRule(firstDose, 2, UpToDate)
  // 2 doses, dose 1 received at or after 1st birthday and dose 2 received less than 12 months ago.
  val rule21 = doseCountRule(2) && doseAfterRule(dob, firstDose, 12) && recentlyRule(secondDose, 12, UpToDate)
  // 2 doses, dose 1 received at or after 1st birthday and dose 2 received more than 12 months ago.
  val rule22 = doseCountRule(2) && doseAfterRule(dob, firstDose, 12) && !recentlyRule(secondDose, 12, Incomplete)
  // 2 doses, child is 7 years or older and dose 2 received less than 12 months ago.
  val rule23 = doseCountRule(2) && olderThanRule(dob, 84) && doseAfterRule(dob, firstDose, 84) && recentlyRule(secondDose, 12, UpToDate)
  override def immunizationStatus (): VaccineStatuses =
  doses.size match {
    case 0 =>
      if (isNewBorn(dob)) UpToDate
      else Incomplete
    case 1 =>
      // Less than 2 months old or child is less than 4 months of age.
      if ((youngerThan(dob, 4)) || (recently(firstDose, 2))) UpToDate
      else Incomplete
    case 2 =>
      // Dose 1 received at or after 1st birthday and dose 2 received less than 12 months ago.
      if ((doseIsAfter(firstDose, dob, 12)) && (recently(secondDose, 12))) UpToDate
      // Dose 1 received at or after 1st birthday and dose 2 received more than 12 months ago.
      if ((doseIsAfter(firstDose, dob, 12)) && (!recently(secondDose, 12))) Incomplete
      // Child is 7 years or older and dose 2 received less than 12 months ago.
      if (((DateTime.now).isAfter(dob.plusMonths(84))) && (recently(secondDose, 12))) UpToDate
      // Child is 7 years or older and dose 2 received more than 12 months ago.
      if ((DateTime.now).isAfter(dob.plusMonths(84)) && (!recently(secondDose, 12))) Incomplete
      // Dose 2 received less than 2 months ago or child is less than 6 months old.
      if ((recently(secondDose, 2)) || (youngerThan(dob, 6))) UpToDate
      // Dose 2 received more than 2 months ago and child is less than 6 months old.
      if ((!recently(secondDose, 2)) && (youngerThan(dob, 6))) UpToDate
      else Incomplete
    case 3 =>
      // Dose received after 7th birthday.
      if (doseIsAfter(thirdDose, dob, 84)) Complete
      // Dose 3 received less than 12 months ago.
      if (recently(thirdDose, 12)) UpToDate
      // Dose 1 received at or after 1st birthday and child is less than 4 years old.
      if (doseIsAfter(firstDose, dob, 12) && youngerThan(dob, 48)) UpToDate
      // Child is less than 18 months old.
      if (youngerThan(dob, 18)) UpToDate
      // Dose 3 recceived 12 months or more ago and child is 18 months old or older.
      else Incomplete
    case 4 =>
      // Dose 4 was given at or after 4th Birthday
      if (doseIsAfter(fourthDose, dob, 48)) Complete
      // Dose 4 was before 4th Birthday and child is less than Kindergarten age (5).
      if (doseIsBefore(fourthDose, dob, 48) && youngerThan(dob, 60)) UpToDate
      // Dose 4 was received before 4th birthday and child is kindergarten of higher grade.
      else Incomplete
    case 5 =>
      Complete
    case default => Error
  }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class HIB (dob: DateTime, doses: DateMap)
    extends Vaccine("HIB", dob, doses, 5)
    with Older
    with Younger
    with NewBorn
    with Recently {
  override def immunizationStatus (): VaccineStatuses =
    doses.size match {
      case 0 =>
        // If child is less than 2 months old, HIB is not needed yet.
        if (isNewBorn(dob)) UpToDate
        // If child is more than 4 years old, it is not required.
        if (olderThan(dob, 48)) Complete
        else Incomplete
      case 1 =>
        // Received less than 2 months ago or child is less than four months old.
        if (recently(firstDose, 2) && youngerThan(dob, 4)) UpToDate
        // Received after 15 months old, age less than 60 months.
        if (doseIsAfter(firstDose, dob, 15) && youngerThan(dob, 60)) Complete
        // Received before 15 months old, first dose less than 2 months ago.
        if (recently(firstDose, 2) && doseIsBefore(firstDose, dob, 15)) UpToDate
        // Received before 15 months old, age less than 60 months.
        if (doseIsBefore(firstDose, dob, 15) && olderThan(dob, 18) && youngerThan(dob, 60))
          Incomplete
        // Not required for 5 years and older.
        if (olderThan(dob, 60)) Complete
        else Incomplete
      case 2 =>
        if (youngerThan(dob, 12)) UpToDate
        // Received second dose after 15 months, age < 18 months.
        if (doseIsAfter(secondDose, dob, 15) && youngerThan(dob, 60)) Complete
        // First dose after 12 months, age < 18 months.
        if (doseIsAfter(firstDose, dob, 12) && youngerThan(dob, 60)) Complete
        // First dose before 12 months, second dose before 15 moontsh, age < 60 months.
        if (doseIsBefore(firstDose, dob, 12) &&
            doseIsBefore(secondDose, dob, 15) &&
            youngerThan(dob, 60)) Incomplete
        // Not required for 5 years and older.
        if (olderThan(dob, 60)) Complete
        else Incomplete
      case 3 =>
        if (youngerThan(dob, 12)) UpToDate
        if (youngerThan(dob, 18) && doseIsAfter(thirdDose, dob, 12)) Complete
        if (youngerThan(dob, 18) && doseIsBefore(thirdDose, dob, 12)) UpToDate
        if (youngerThan(dob, 60) && doseIsAfter(thirdDose, dob, 12)) Complete
        if (youngerThan(dob, 60) && doseIsBefore(thirdDose, dob, 12)) UpToDate
        if (olderThan(dob,60)) Complete
        else Incomplete
      case 4 => Complete
      case default => Error
  }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class Polio (dob: DateTime, doses: DateMap)
    extends Vaccine("polio", dob, doses, 5)
    with Younger
    with Recently
    with Older {
  override def immunizationStatus (): VaccineStatuses =
    doses.size match {
      case 0 =>
        // If child is less than 2 months old, Polio vaccine is not needed yet.
        if (youngerThan(dob, 2)) UpToDate
        // If child is more than 18 years old, Polio vaccine is not needed yet.
        if (olderThan(dob, 18 * 12)) UpToDate
        else Incomplete
      case 1 =>
        // Dose 1 received less than 2 months ago or child is less than 4 months old.
        if (recently(firstDose, 2) || youngerThan(dob, 4)) UpToDate
        // Dose 1 received more than 2 months ago and child is less than 4 months old.
        if (youngerThan(dob, 18 * 12)) Incomplete
        else Complete
      case 2 =>
        // Dose 2 received less than 12 months ago.
        if (recently(secondDose, 12)) UpToDate
        // Child is less that 18 months old
        if (youngerThan(dob, 18)) UpToDate
        else Incomplete
      case 3 =>
        // Dose 3 received after 4 years
        if (doseIsAfter(thirdDose, dob, 4 * 12)) Complete
        // Dose 3 received less that 12 months ago
        if (recently(thirdDose, 12)) UpToDate
        // Child is less than kindergarten age (5)
        if (youngerThan(dob, 5 * 12)) UpToDate
        // Dose 3 received more than 12 months ago, and child is kindergarten or
        // higher age, but less than 18 years old.
        else Incomplete
      case 4 => Complete
    }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class Varicella (dob: DateTime, diseaseHistory: List[String], doses: DateMap)
    extends Vaccine("varicella", dob, doses, 2)
    with Younger
    with Older
    with Recently {
  override def immunizationStatus (): VaccineStatuses =
    if (diseaseHistory contains "Chicken Pox") Complete
    else
      doses.size match {
        case 0 =>
          // No received and child is less than 18 months old.
          if (youngerThan(dob, 18)) UpToDate
          else Incomplete
        case 1 =>
          // First dose is after age 12 months
          if (doseIsAfter(firstDose, dob, 12)) Complete
          // Received before 12 months of age and child is less than 18 months old.
          if (doseIsBefore(firstDose, dob, 12) && youngerThan(dob, 18)) UpToDate
          // Received before 12 months of age and child is more than 18 months old.
          if (doseIsBefore(firstDose, dob, 12) && olderThan(dob, 18)) Incomplete
          // Received at or after 13 years of age and less than two months ago.
          if (olderThan(dob, 13 * 12) && recently(firstDose, 2)) UpToDate
          // Received at or after 13 years of age and less than two months ago.
          if (olderThan(dob, 13 * 12) && !recently(firstDose, 2)) Incomplete
          else Incomplete
        case 2 =>
          // At least one dose given between 12 months of age and 12 years of age
          if (doseIsAfter(firstDose, dob, 12) && doseIsBefore(firstDose, dob, 12 * 12)) Complete
          if (doseIsAfter(secondDose, dob, 12) && doseIsBefore(secondDose, dob, 12 * 12)) Complete
          // First dose given after age 13 and second dose given more than 24
          // days after the first dose.
          if (doseIsAfter(firstDose, dob, 13 * 12) && doseAfterDose(firstDose, secondDose, 24)) Complete
          // Second dose given fewer than 24 days after the first dose and less
          // than two months ago.
          if (doseBeforeDose(firstDose, secondDose, 24) && recently(secondDose, 2)) UpToDate
          // Second dose prior to 12 months of age
          if (doseIsBefore(secondDose, dob, 12) && olderThan(dob, 12)) Incomplete
          else Incomplete
        case 3 => Complete
        case default => Error
      }
}

// Vaccination status rules for MMR (Measles, Mumps, and Rubella)
class MMR (dob: DateTime, doses: DateMap)
    extends Vaccine("mmr", dob, doses, 2)
    with Younger
    with Older
    with Recently {
  override def immunizationStatus (): VaccineStatuses =
    if (youngerThan(dob, 5 * 12))
      doses.size match {
        case 0 =>
          // Child is under 15 months old
          if (youngerThan(dob, 15)) UpToDate
          else Incomplete
        case 1 =>
          // **** Need to restrict this to less than K age.
          // Received after 12 months of age.
          if (doseIsAfter(firstDose, dob, 12)) Complete
          // Received prior to 12 months of age and child is under 15 months of age.
          if (doseIsBefore(firstDose, dob, 12) && youngerThan(dob, 15)) UpToDate
          else Incomplete
        case default => Complete
      } // match
    else
      doses.size match {
        case 0 => Incomplete
        case 1 =>
          // Received less that two months ago.
          if (recently(firstDose, 2)) UpToDate
          else Incomplete
        case 2 =>
          // Received after 12 months of age.
          if (doseIsAfter(firstDose, dob, 12) && doseAfterDose(firstDose, secondDose, 24)) Complete
          // First dose received prior to 12 months of age and second
          // received less than two months ago.
          if (doseIsBefore(firstDose, dob, 12) && recently(secondDose, 2)) UpToDate
          // First does received prior to 12 months of age and second
          // dose received two or more months ago.
          if (doseIsBefore(firstDose, dob, 12) && !recently(secondDose, 2)) Incomplete
          // Second dose given fewer than 24 days after the first
          // dose and less than two months ago.
          if (!doseAfterDose(firstDose, secondDose, 24) && !recently(secondDose, 2)) UpToDate
          // Second dose given fewer than 24 days after first dose and two or more months ago.
          if (!doseAfterDose(firstDose, secondDose, 24) && recently(secondDose, 2)) Incomplete
          else Incomplete
        case 3 => Complete
      } // match
}

// Vaccination status rules for HEPA (Hepatitis A)
class HEPA (dob: DateTime, doses: DateMap)
    extends Vaccine("hepA", dob, doses, 4)
    with Younger
    with Recently {
  override def immunizationStatus (): VaccineStatuses =
    doses.size match {
      case 0 =>
        // Child is under 18 months old
        if (youngerThan(dob, 18)) UpToDate
        else Incomplete
      case 1 =>
        // Received after 12 months of age and less than 12 months ago.
        if (doseIsAfter(firstDose, dob, 12) && recently(firstDose, 12)) UpToDate
        // Received after 12 months of age and more that 12 months ago.
        if (doseIsAfter(firstDose, dob, 12) && !recently(firstDose, 12)) Incomplete
        // Received prior to 12 months of age and child is less that 18 months of age.
        if (doseIsBefore(firstDose, dob, 12) && youngerThan(dob, 18)) UpToDate
        // Recieved prior to 12 months of age and child is 18 months or older.
        else Incomplete
      case 2 =>
        // Both doses received after 12 months of age.
        if (doseIsAfter(firstDose, dob, 12) && doseIsAfter(secondDose, dob, 12)) Complete
        // Dose 1 received prior to 12 months of age and dose 2
        // received less than 2 months ago.
        if (doseIsBefore(firstDose, dob, 12) && recently(secondDose, 12)) UpToDate
        // Dose 1 received priot to 12 months of age and dose 2
        // received 12 months of more ago.
        else Incomplete
      case 3 => Complete
      case 4 => Complete
  }
}

// Vaccination status rules for HEPB (Hepatitis B)
class HEPB (dob: DateTime, doses: DateMap)
    extends Vaccine("hepB", dob, doses, 3)
    with Younger
    with Older
    with Recently {
  override def immunizationStatus (): VaccineStatuses =
    doses.size match {
      case 0 =>
        // Child is under 2 months old.
        if (youngerThan(dob, 2)) UpToDate
        // Child is 2 months old or older.
        else Incomplete
      case 1 =>
        // Received at or after 11 years old and less than 6 months ago.
        if (olderThan(dob, 11 * 12) && recently(firstDose, 6)) UpToDate
        // Received at or after 11 years of age and 6 months or more ago.
        if (olderThan(dob, 11 * 12) && !recently(firstDose, 6)) Incomplete
        // Received before 11 years of age and less than 2 months ago or
        // child is less than 4 months old.
        if ((doseIsBefore(firstDose, dob, 11 * 12) &&
             recently(firstDose, 2)) || youngerThan(dob, 4)) UpToDate
        // Received before 11 years of age and more than 2 months ago
        // and child is 4 months old or older.
        if ((doseIsBefore(firstDose, dob, 11 * 12) &&
            !recently(firstDose, 2)) &&
            olderThan(dob, 4)) Incomplete
        else Incomplete
      case 2 =>
        // First dose received at or after 11 years old and second dose
        // received at least 4 months after first does.
        if (doseIsBefore(firstDose, dob, 11 * 12) && doseAfterDose(firstDose, secondDose, 4)) Complete
        // First dose is received at or after 18 years old.
        if (doseIsAfter(firstDose, dob, 18 * 12)) Complete
        // Second dose received less that 5 months ago
        if (recently(secondDose, 5)) UpToDate
        // Received 5 months or more ago and child is less than 18 months old.
        if (!recently(secondDose, 5) && youngerThan(dob, 18)) UpToDate
        // Received 5 months or more ago and child is more than 18 months old.
        else Incomplete
      case 3 => Complete
      case 4 => Complete
    }
}

class Student(jsonMap: JsonMap){
  def filterShots(jm: JsonMap, shotRegex: String): JsonMap =
    jm.filterKeys(_.matches(shotRegex))
  val dob: DateTime = new DateTime(jsonMap("dob"))
  val fullName = jsonMap("firstName") + " " + jsonMap("lastName")

  // Date parser to keep us from throwing exceptions.
  def validateDate(date: String) =
    try {
      val df = new SimpleDateFormat("yyyy-MM-dd")
      df.setLenient(false)
      df.parse(date)
      true
    } catch { case e: Any => false }

  // Make sure all date strings in a map are valid.
  def validateDates(dm: JsonMap): Boolean = dm.map(kv => validateDate(kv._2)).forall(identity)

  // Convert filtered map with valid data strings to a map of DateTime.
  // We check that all of the date string are valid, so we can call new DateTime
  // without fear of throwing an exception.
  // If at least one of the dates is invalid, we return an empty Map, thus
  // the entire set of shots for a given vaccine is rejected. An error message
  // is given for the immunization administrator.
  def convertDateStrings(filtered: JsonMap): DateMap = {
    if (!validateDates(filtered)) {
      println(" There is an invalid date for student " +
        fullName + "in the following sequence of shots\n" + filtered)
      Map.empty
    } else { filtered.map(kv => (kv._1, new DateTime(kv._2))) }
  }

  val dtapShots: DateMap = convertDateStrings(filterShots(jsonMap, "dtap.*"))
  val polioShots: DateMap = convertDateStrings(filterShots(jsonMap, "polio.*"))
  val mmrShots: DateMap = convertDateStrings(filterShots(jsonMap, "mmr.*"))
  val varicellaShots: DateMap =
    convertDateStrings(filterShots(jsonMap, "varicella.*"))
  val hibShots: DateMap = convertDateStrings(filterShots(jsonMap, "hib.*"))
  val hepaShots: DateMap = convertDateStrings(filterShots(jsonMap, "hepA.*"))
  val hepbShots: DateMap = convertDateStrings(filterShots(jsonMap, "hepB.*"))

  // Validate the shots.
  val dtap: DTAP = new DTAP(fullName, dob, dtapShots)
  val dtapStatus: VaccineStatuses = dtap.immunizationStatus()
  val hib: HIB = new HIB(dob, hibShots)
  val hibStatus: VaccineStatuses = hib.immunizationStatus()
  val polio: Polio = new Polio(dob, polioShots)
  val polioStatus: VaccineStatuses = polio.immunizationStatus()
  val varicella: Varicella = new Varicella(dob, List(), varicellaShots)
  val varicellaStatus: VaccineStatuses = varicella.immunizationStatus()
  val mmr: MMR = new MMR(dob, mmrShots)
  val mmrStatus: VaccineStatuses = mmr.immunizationStatus()
  val hepa: HEPA = new HEPA(dob, hepaShots)
  val hepaStatus: VaccineStatuses = hepa.immunizationStatus()
  val hepb: HEPB = new HEPB(dob, hepbShots)
  val hepbStatus: VaccineStatuses = hepb.immunizationStatus()
  val gatherStatus: List[VaccineStatuses] = List(dtapStatus,
    hibStatus, polioStatus, varicellaStatus, mmrStatus, hepaStatus, hepbStatus)
  // Summary Status
  // val studentImmunizationStatus = summaryStatus(gatherStatus)

  // Print out a student.
  def printStudent():Unit = {
    println("Name: " + fullName + "\t\tDOB: " + dob)
    println("DTAP Shots: \t" + dtapShots)
    println("Polio Shots: \t" + polioShots)
    println("MMR Shots: \t" + mmrShots)
    println("Varicella Shots: \t" + varicellaShots)
    println("HIB Shots: \t" + hibShots)
    println("Hepatitus A Shots: \t" + hepaShots)
    println("Hepatitus B Shots: \t" + hepbShots)
  }
}

class Students (students: JsonMaps) {
  def processOneStudent(studentJson: JsonMap): (String, VaccineStatuses) = {
    val student: Student = new Student(studentJson)
    val statuses:List[VaccineStatuses] = student.gatherStatus
    val summary: VaccineStatuses = summaryStatus(statuses)
    (student.fullName, summary)
  }

  def processStudents(students: JsonMaps): List[(String, VaccineStatuses)] =
    students.map(s => processOneStudent(s))
}

// ----------------------------------------------------------------------------
object Main extends App {
  override def main(args: Array[String]): Unit = {

    // Object to parse the JSON information.
    val parser = new json.JsonDoses ("inputs/ImmunizationData.json")
    val doseMaps: JsonMaps = parser.mapify("inputs/ImmunizationData.json")
    val x1 = doseMaps(0)("dtap1")
    val x2 = doseMaps(1)("dtap2")
    println("======= > x1, x2 = ", x1, x2)

    val sample: String = """[{
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

    // The empty "dtap5" for Aaditi does not show up in the map, so no need for null values.
    val sampleAst: spray.json.JsValue = sample.parseJson
    val sampleJson: String = sampleAst.prettyPrint
    println("\n------ sampleJson > " + sampleJson)
    val sampleMap: JsonMaps = sampleAst.convertTo[JsonMaps]
    println("\n------ sampleMap > " + sampleMap)
    val student1: JsonMap = sampleMap(0)
    println("\n------ student1 > " + student1)
    val sampleStudents = new Students(sampleMap)
    println("\n------ sampleStudents\n")
    val processedStudents : List[(String, VaccineStatuses)] =
      sampleStudents.processStudents(sampleMap)
    def outProcessedStudent(p: (String, VaccineStatuses)) = (p._1, outStatus(p._2))
    println("\n------ processedStudents > " + processedStudents.map(p => outProcessedStudent(p)))

    // println("\n------ 2000 characters from > " + filename)
    // print(glob.take(2000))
    // println("\n------ parsedJson > " + jsonImmunizations.prettyPrint)
    // println("\n------ jsonListMap > " + jsonImmunizations.prettyPrint)
    // val jsonListMap = jsonAst.convertTo[JsonMaps]
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
