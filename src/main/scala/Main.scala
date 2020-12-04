// Can only declare a package for scalac compilation.
// This is ignored in the REPL, it just generates a warning.
// package com.twitter.example
// package scala.language.implicitConversions
// import cats.effect.IO
import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
import scala.io.Source._
import org.joda.time.base.AbstractInstant
import org.joda.time.DateTime
import org.joda.time.Period

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
      case Some(d) => (d.isAfter(dob.plusMonths(startMonth)) && (d.isBefore(dob.plusMonths(endMonth))))
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

trait NewBorn {
  // Determine if the child is new born.
  def isNewBorn (dob: DateTime): Boolean = dob.plusMonths(2).isAfter(DateTime.now())
}


// Common operations on the doses given.
class Doses(name: String, dob: DateTime, doses: Array[Option[DateTime]]) extends Enumeration with NewBorn with AgeRange {
  // Compute the number of doses of the vaccine given.
  val currentDate = new DateTime() // For some reason this give the current time.
  def numberOfDoses(doses: Array[Option[DateTime]]): Int = doses.count(d => !d.isEmpty)
  def doseNwithinAgePeriod(dose: Int, startMonth: Int, endMonth: Int): Boolean =
    withinRange(doses(dose), dob, startMonth, endMonth)
  def doseIsAfter(dose: DateTime, dob: DateTime, nMonths: Int): Boolean = dose.isAfter(dob.plusMonths(nMonths))
  def doseIsAfter(dose: Option[DateTime], dob: DateTime, nMonths: Int): Boolean =
    dose match {
      case None => false
      case Some(dose) => dose.isAfter(dob.plusMonths(nMonths))
    }
  def doseIsBefore(dose: DateTime, dob: DateTime, nMonths: Int): Boolean = dose.isBefore(dob.plusMonths(nMonths))
  def doseIsBefore(dose: Option[DateTime], dob: DateTime, nMonths: Int): Boolean =
    dose match {
      case None => false
      case Some(dose) => dose.isBefore(dob.plusMonths(nMonths))
    }

}

// Vaccinations
// Example names: DTAP, with maximum of 5 shots (vaccinations).
//                Polio, with maximum of 5 shots (vaccinations).
class Vaccine(name: String, max: Int) extends VaccineStatus {
  def fillDoses(doses: Array[DateTime], max: Int): Array[DateTime] = {
    for (j <- 0 to 10) { doses(j) = null }
    return null
  }
  val doses: Array[DateTime] = fillDoses(doses, max)
  println ("doses.length: ", doses.length)
}

// Vaccination status rules for DTAP (Diptheria, Tetanus, Pertussis)
// Other abbreviations used are DTP, DTap, DT, Td, Tdap.
class DTAP (dob: DateTime, doses: Array[Option[DateTime]]) extends Doses("DTAP", dob, doses) with Younger with VaccineStatus with Recently with NewBorn {
  def immunizationStatus (): VaccineStatus =
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
class HIB (dob: DateTime, doses: Array[Option[DateTime]]) extends Doses("DTAP", dob, doses) with Older with Younger with VaccineStatus with NewBorn with Recently {
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
        if (doseIsBefore(dose1, dob, 15) && olderThan(dob, 18) && youngerThan(dob, 60)) Incomplete
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
        if (doseIsBefore(dose1, dob, 12) && doseIsBefore(dose2, dob, 15) && youngerThan(dob, 60)) Incomplete
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

// ----------------------------------------------------------------------------
object ImmunizationReport {
  def main(args: Array[String]): Unit = {

    // JSON
    val source = """{ "some": "JSON source", "other": "XML" }"""
    val jsonAst: spray.json.JsValue = source.parseJson // or JsonParser(source)
    val json = jsonAst.prettyPrint // or .compactPrint
    val jsonAst2 = List(1, 2, 3).toJson
    val myObject = jsonAst.convertTo[Map[String,String]]
    println("\nsource = " + source)
    println("jsonAst = " + jsonAst)
    println("json = " + json)
    println("jsonAst2 = " + jsonAst2)
    println("myObject = " + myObject)
    println("myObject(\"some\") = " + myObject("some"))

    val filename = "fileopen.scala"
    println("------ > inputs/ImmunizationData.json\n")
    var j = 0
    for (line <- fromFile("inputs/ImmunizationData.json").getLines) {
      if (j < 12) {
        println(line)
        j = j + 1
      }
    }
  }
}

// List<String> SCOPES_ARRAY = Arrays.asList(
//         "https://www.googleapis.com/auth/drive.file",
//         "https://www.googleapis.com/auth/userinfo.email",
//         "https://www.googleapis.com/auth/userinfo.profile",
//         "https://docs.google.com/feeds",
//         "https://spreadsheets.google.com/feeds");
// http://blog.pamelafox.org/2013/06/exporting-google-spreadsheet-as-json.html
