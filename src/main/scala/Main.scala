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
  val Incomplete, UpToDate, Complete, None = Value
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
  def olderThan(numberOfMonths: Int, dob: DateTime): Boolean =
    dob.plusMonths(numberOfMonths).isBefore(DateTime.now())
}

trait Younger {
  def youngerThan(numberOfMonths: Int, dob: DateTime): Boolean =
    dob.plusMonths(numberOfMonths).isAfter(DateTime.now())
}

trait Recently {
  def recently(numberOfMonths: Int, dose: DateTime): Boolean =
    DateTime.now().plusMonths(-numberOfMonths).isBefore(dose)
  def recently(numberOfMonths: Int, dose: Option[DateTime]): Boolean =
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

class DTAP (dob: DateTime, doses: Array[Option[DateTime]]) extends Doses("DTAP", dob, doses) with Younger with VaccineStatus with Recently with NewBorn {
  def immunizationStatus (): VaccineStatus =
  numberOfDoses(doses) match {
    case 0 =>
      if (isNewBorn(dob)) UpToDate
      else Incomplete
    case 1 =>
      // Less than 2 months old or child is less than 4 months of age.
      if ((youngerThan(4, dob)) || (recently(2, doses(0)))) UpToDate
      else Incomplete
    case 2 =>
      // Dose 1 received at or after 1st birthday and dose 2 received lesss than 12 months ago.
      if ((doseIsAfter(doses(1), dob, 12)) && (recently(12,doses(1)))) UpToDate
      else Incomplete
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
