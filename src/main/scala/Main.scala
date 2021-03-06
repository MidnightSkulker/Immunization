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
// import org.apache.log4j._
// import org.apache.logging.log4j.scala.Logging
// import org.apache.logging.log4j.scala.Logger._
// import org.apache.logging.log4j._
import org.slf4j.LoggerFactory

import models.model._
import json._
import rules._
import doses._

// Vaccinations
// Example names: DTAP, with maximum of 5 shots (vaccinations).
//                Polio, with maximum of 5 shots (vaccinations).
abstract class Vaccine(name: String,
  dob: DateTime,
  doses: DateMap,
  max: Int) extends Doses(name: String, dob: DateTime, doses: DateMap) {
  def immunizationStatus (): RulesResult = new RulesResult()
}

// Vaccination status rules for DTAP (Diptheria, Tetanus, Pertussis)
// Other abbreviations used are DTP, DTap, DT, Td, Tdap.
class DTAP (name: String, dob: DateTime, doses: DateMap)
    extends Vaccine("dtap", dob, doses, 5) with SpecificRules {

  private val logger = LoggerFactory.getLogger(classOf[DTAP])

  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class DTAP immunization Status ----------")

    val rule01: Rule = doseCountRule(doses, 0) && newBornRule(dob, UpToDate)
    val rule02: Rule = doseCountRule(doses, 0) && !newBornRule(dob, Incomplete)
    // One dose, first dose less than 2 months old or
    // child is less than 4 months of age.
    val rule11 =
      doseCountRule(doses, 1) && youngerThanRule(dob, 4) && recentlyRule(firstDose, 2, UpToDate)
    // One dose, first does more than two months ago or child less than 4 months old.
    val rule12 =
      doseCountRule(doses, 1) && (!youngerThanRule(dob, 4) || !recentlyRule(firstDose, 2, UpToDate))
    // 2 doses, dose 1 received at or after 1st birthday and
    // dose 2 received less than 12 months ago.
    val rule21 = doseCountRule(doses, 2) && doseAfterRule(dob, firstDose, 12) && recentlyRule(secondDose, 12, UpToDate)
    // 2 doses, dose 1 received at or after 1st birthday and
    // dose 2 received more than 12 months ago.
    val rule22 = doseCountRule(doses, 2) && doseAfterRule(dob, firstDose, 12) && !recentlyRule(secondDose, 12, Incomplete)
    // 2 doses, child is 7 years or older and dose 2 received less than 12 months ago.
    val rule23 = doseCountRule(doses, 2) && olderThanRule(dob, 84) && doseAfterRule(dob, firstDose, 84) && recentlyRule(secondDose, 12, UpToDate)
    // 2 doses, child is 7 years or older and dose 2 received more than 12 months ago.
    val rule24 = doseCountRule(doses, 2) && olderThanRule(dob, 84) && doseAfterRule(dob, firstDose, 84) && (!recentlyRule(secondDose, 12, Incomplete))
    // 2 doses, dose 2 received less than 2 months ago or
    // child is less than 6 months old.
    val rule25 = doseCountRule(doses, 2) && recentlyRule(secondDose, 2) && youngerThanRule(dob, 6, Incomplete)
    // 2 doses, dose 2 received more than 2 months ago or
    // child is less than 6 months old.
    val rule26 = doseCountRule(doses, 2) && !recentlyRule(secondDose, 2) && youngerThanRule(dob, 6, Incomplete)
    // 3 doses, third dose received after 7th birthday.
    val rule31 = doseCountRule(doses, 3) && doseAfterRule(dob, thirdDose, 84, Complete)
    // 3 doses, third dose received less than 12 months ago.
    val rule32 = doseCountRule(doses, 3) && recentlyRule(thirdDose, 12, UpToDate)
    // 3 doses, first does received at or after 1st birthday and child is less than 4 years old.
    val rule33 = doseCountRule(doses, 3) && doseAfterRule(dob, firstDose, 12) && youngerThanRule(dob, 48, UpToDate)
    // 3 doses, child is less than 18 months old.
    val rule34 = doseCountRule(doses, 3) && youngerThanRule(dob, 18, UpToDate)
    // 3 doses, third dose recceived 12 months or more ago and child is 18 months old or older.
    val rule35 = doseCountRule(doses, 3) && (!recentlyRule(thirdDose, 12) || olderThanRule(dob, 18, Incomplete))
    // 4 doses, fourth Dose was given at or after 4th Birthday
    val rule41 = doseCountRule(doses, 4) && doseAfterRule(dob, firstDose, 48, Complete)
    // 4 doses, fourth dose was before 4th Birthday and child is less than Kindergarten age (5).
    val rule42 = doseCountRule(doses, 4) && doseBeforeRule(dob, fourthDose, 48) && youngerThanRule(dob, 60, UpToDate)
    // 4 doses, fourth dose was received before 4th birthday and
    // child is kindergarten of higher grade.
    val rule43 = doseCountRule(doses, 4) && doseBeforeRule(dob, fourthDose, 48) && olderThanRule(dob, 60, Incomplete)
    val rule51 = doseCountRule(doses, 5, Complete)

    val rules: Rules = new Rules(
      rules = List(rule01, rule02, rule11, rule12, rule21, rule22, rule23,
        rule24, rule25, rule26, rule31, rule32, rule33, rule34,
        rule35, rule41, rule42, rule43, rule51))
    val decision: RulesResult = rules.documentedDecision()
    println("DTAP Decision: " + outStatus(decision.finalStatus()))
    logger.info("DTAP Decision: " + outStatus(decision.finalStatus()))
    return decision
  }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class HIB (dob: DateTime, doses: DateMap)
    extends Vaccine("hib", dob, doses, 5) with SpecificRules {
  private val logger = LoggerFactory.getLogger(classOf[HIB])

  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class HIB immunization Status ----------\n\n")
    // Zero doses, child is less than 2 months old, HIB is not needed yet.
    val rule01: Rule = doseCountRule(doses, 0) && newBornRule(dob, UpToDate)
    // Zero doses, child is more than 4 years old, it is not required.
    val rule02: Rule = doseCountRule(doses, 0) && olderThanRule(dob, 48, UpToDate)
    val rule03: Rule = doseCountRule(doses, 0) && !newBornRule(dob) && !olderThanRule(dob, 48, Incomplete)
    // One dose, first dose less than 2 months old and
    // child is less than 4 months of age.
    val rule11 =
      doseCountRule(doses, 1) && youngerThanRule(dob, 4) && recentlyRule(firstDose, 2, UpToDate)
    // One dose, first does more than two months ago or child less than 4 months old.
    val rule12 =
      doseCountRule(doses, 1) && (!youngerThanRule(dob, 4) || !recentlyRule(firstDose, 2, UpToDate))
    // One dose, received after 15 months old, age less than 60 months.
    val rule13 = doseCountRule(doses, 1) && youngerThanRule(dob, 60) && doseAfterRule(dob, firstDose, 15, Complete)
    // Received before 15 months old, first dose less than 2 months ago.
    val rule14 = doseCountRule(doses, 1) && recentlyRule(firstDose, 2) && doseBeforeRule(dob, firstDose, 15, UpToDate)
    // Received before 15 months old, age less than 60 months.
    val rule15 = doseCountRule(doses, 1) && doseBeforeRule(dob, firstDose, 15) && youngerThanRule(dob, 60, Incomplete)
    // End 1 dose
    val rule16 = doseCountRule(doses, 1) && youngerThanRule(dob, 60, Incomplete)
    // 2 doses, younger than 12 months.
    val rule21 = doseCountRule(doses, 2) && youngerThanRule(dob, 12, Complete)    
    // 2 doses, received second dose after 15 months, age < 18 months.
    val rule22 = doseCountRule(doses, 2) && doseAfterRule(dob, secondDose, 15) && youngerThanRule(dob, 18, Complete)
    // 2 doses, first dose after 12 months, age < 18 months.
    val rule23 = doseCountRule(doses, 2) && doseAfterRule(dob, firstDose, 12) && youngerThanRule(dob, 60, Complete)
    // 2 doses, first dose before 12 months, second dose before 15 moontsh, age < 60 months.
    val rule24 = doseCountRule(doses, 2) && doseBeforeRule(dob, firstDose, 12) && doseBeforeRule(dob, secondDose, 15) && youngerThanRule(dob, 60, Incomplete)
    // 2 doses, else
    val rule25 = doseCountRule(doses, 2, Incomplete)
    val rule31 = doseCountRule(doses, 3) && youngerThanRule(dob, 12, UpToDate)
    val rule32 = doseCountRule(doses, 3) && youngerThanRule(dob, 18) && doseAfterRule(dob, thirdDose, 12, Complete)
    val rule33 = doseCountRule(doses, 3) && youngerThanRule(dob, 18) && doseBeforeRule(dob, thirdDose, 12, UpToDate)
    val rule34 = doseCountRule(doses, 3) && youngerThanRule(dob, 60) && doseAfterRule(dob, thirdDose, 12, Complete)
    val rule35 = doseCountRule(doses, 3) && youngerThanRule(dob, 60) && doseBeforeRule(dob, thirdDose, 12, UpToDate)
    val rule36 = doseCountRule(doses, 3, Incomplete)
    // Not required for 5 years and older.
    // 4 doses complete at any age.
    val rule41 = doseCountRule(doses, 4, Complete) || olderThanRule(dob, 60, Complete)
    val rules: Rules = new Rules(
      rules = List(rule01, rule02, rule03,
        rule11, rule12, rule13, rule14, rule15, rule16,
        rule21, rule22, rule23, rule24, rule25,
        rule31, rule32, rule33, rule34, rule35, rule36, rule41))
    val decision: RulesResult = rules.documentedDecision()
    println("HIB Decision: " + outStatus(decision.finalStatus()))
    logger.info("HIB Decision: " + outStatus(decision.finalStatus()))
    return decision
  }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class Polio (dob: DateTime, doses: DateMap)
    extends Vaccine("polio", dob, doses, 5) with SpecificRules {
  private val logger = LoggerFactory.getLogger(classOf[Polio])
  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class Polio immunization Status ----------\n\n")
    // If child is more than 18 years old, Polio vaccine is not needed yet.
    val rule18: Rule = olderThanRule(dob, 18 * 12, Complete)
    val rule01: Rule = doseCountRule(doses, 0) && newBornRule(dob, UpToDate)
    val rule02: Rule = doseCountRule(doses, 0) && !newBornRule(dob, Incomplete)
    // 1 dose, first dose less than 2 months ago or child is less than 4 months old.
    val rule11: Rule = recentlyRule(firstDose, 2) || youngerThanRule(dob, 4, UpToDate)
    // 1 dose,  first dose more than 2 months ago and child is less than 4 months old.
    val rule12: Rule = doseCountRule(doses, 1) && !recentlyRule(firstDose, 2) && youngerThanRule(dob, 4, Incomplete)
    // 1 dose,  child is less than 18 years old.
    val rule13: Rule = doseCountRule(doses, 1) && youngerThanRule(dob, 18 * 12, Incomplete)
    // 2 doses, second dose received less than 12 months ago.
    val rule21: Rule = doseCountRule(doses, 2) && recentlyRule(secondDose, 12, UpToDate)
    // 2 doses, child is less than 18 months old.
    val rule22: Rule = doseCountRule(doses, 2) && youngerThanRule(dob, 18, UpToDate)
    // 2 doses, child is more than 18 months old.
    val rule23: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 18, Incomplete)
    // 3 doses, third dose received after 4 years
    val rule31: Rule = doseCountRule(doses, 3) && doseAfterRule(dob, thirdDose, 4 * 12, Complete)
    // 3 doses, third dose received less that 12 months ago
    val rule32: Rule = doseCountRule(doses, 3) && recentlyRule(thirdDose, 12, UpToDate)
    // 3 doses, child is less than kindergarten age (5)
    val rule33: Rule = doseCountRule(doses, 3) && youngerThanRule(dob, 5 * 12, UpToDate)
    // 3 doses, third dose received more than 12 months ago, and child is kindergarten or
    // higher age, but less than 18 years old.
    val rule34: Rule = doseCountRule(doses, 3) && !youngerThanRule(dob, 5 * 12, Incomplete)
    // 4 doses
    val rule41: Rule = doseCountRule(doses, 4, Complete)

    val rules: Rules = new Rules(
      rules = List(rule18, rule01, rule02, rule11, rule12, rule13,
        rule21, rule22, rule23, rule31, rule32, rule33, rule34, rule41))
    val decision: RulesResult = rules.documentedDecision()
    println("Polio Decision: " + outStatus(decision.finalStatus()))
    logger.info("Polio Decision: " + outStatus(decision.finalStatus()))
    return decision
    }
}

// Vaccination status rules for HIB (Haemophilus Influenza type B)
class Varicella (dob: DateTime, diseaseHistory: List[String], doses: DateMap)
    extends Vaccine("varicella", dob, doses, 2) with SpecificRules {
  private val logger = LoggerFactory.getLogger(classOf[Varicella])
  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class Varicella immunization Status ----------\n\n")
    val ruleChickenPox = diseaseHistoryRule("Chicken Pox", diseaseHistory, Complete)
    // 0 doses, child is less than 18 months old.
    val rule01: Rule = doseCountRule(doses, 0) && youngerThanRule(dob, 18, UpToDate)
    val rule02: Rule = doseCountRule(doses, 0) && !youngerThanRule(dob, 18, Incomplete)
    // First dose is after age 12 months
    val rule11: Rule = doseCountRule(doses, 1) && doseAfterRule(dob, firstDose, 12, Complete)
    // 1 dose, received before 12 months of age and child is less than 18 months old.
    val rule12: Rule = doseCountRule(doses, 1) && doseBeforeRule(dob, firstDose, 12) && youngerThanRule(dob, 18, UpToDate)
    // 1 dose, first dose before 12 months of age and child is more than 18 months old.
    val rule13: Rule = doseCountRule(doses, 1) && doseBeforeRule(dob, firstDose, 12) && olderThanRule(dob, 18, Incomplete)
    // 1 dose, first dose at or after 13 years of age and less than two months ago.
    val rule14: Rule = doseCountRule(doses, 1) && olderThanRule(dob, 13 * 12) && recentlyRule(firstDose, 2, UpToDate)
    // 1 dose, first dose at or after 13 years of age and less than two months ago.
    val rule15: Rule = doseCountRule(doses, 1) && olderThanRule(dob, 13 * 12) && !recentlyRule(firstDose, 2, Incomplete)
    val rule16: Rule = doseCountRule(doses, 1, Incomplete)
    // 2 doses, At least one dose given between 12 months of age and 12 years of age ?????
    val rule21: Rule = doseCountRule(doses, 2) && doseAfterRule(dob, firstDose, 12) && doseBeforeRule(dob, firstDose, 12 * 12, Complete)
    // 2 doses, First dose given after age 13 and second dose given more than 24
    // days after the first dose.
    val rule22: Rule = doseCountRule(doses, 2) && (doseAfterRule(dob, firstDose, 13 * 12) && doseAfterDoseRule(firstDose, secondDose, 24, Complete))
    // 2 doses, Second dose given fewer than 24 days after the first dose and less
    val rule23: Rule =  doseCountRule(doses, 2) && doseBeforeDoseRule(firstDose, secondDose, 24) && recentlyRule(secondDose, 2, UpToDate)
    // 2 doses Second dose prior to 12 months of age
    val rule24: Rule = doseCountRule(doses, 2) && doseBeforeRule(dob, secondDose, 12) && olderThanRule(dob, 12, Incomplete)
    val rule31: Rule = doseCountRule(doses, 3, Complete)

    val rules: Rules = new Rules(
      rules = List(ruleChickenPox, rule01, rule02, rule11, rule12, rule13, rule14, rule15,
        rule21, rule22, rule23, rule24, rule31))
    val decision: RulesResult = rules.documentedDecision()
    println("Varicella Decision: " + outStatus(decision.finalStatus()))
    logger.info("Varicella Decision: " + outStatus(decision.finalStatus()))
    return decision
  }
}

// Vaccination status rules for MMR (Measles, Mumps, and Rubella)
class MMR (dob: DateTime, doses: DateMap)
  extends Vaccine("mmr", dob, doses, 2) with SpecificRules {
  private val logger = LoggerFactory.getLogger(classOf[MMR])
  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class MMR immunization Status ----------\n\n")
    // 0 doses, child is under 15 months old
    val rule01: Rule = doseCountRule(doses, 0) && youngerThanRule(dob, 15, UpToDate)
    // 0 doses, child is more than 15 months old
    val rule02: Rule = doseCountRule(doses, 0) && !youngerThanRule(dob, 15, Incomplete)
    // 1 does, child less that Kindergarten age, received after 12 months of age.
    val rule11: Rule = doseCountRule(doses, 1) && youngerThanRule(dob, 5 * 12) && doseAfterRule(dob, firstDose, 12, Complete)
    // 1 dose, received prior to 12 months of age and child is under 15 months of age.
    val rule12: Rule = doseCountRule(doses, 1) && youngerThanRule(dob, 15) && doseBeforeRule(dob, firstDose, 12, Complete)
    // 1 dose, received after 12 months of age and child is under 15 months of age.
    val rule13: Rule = doseCountRule(doses, 1) && youngerThanRule(dob, 15) && doseAfterRule(dob, firstDose, 12, Incomplete)
    // 1 dose, child more than Kindergarten age, first dose received less than 2 months ago.
    val rule14: Rule = doseCountRule(doses, 1) && !youngerThanRule(dob, 5 * 12) && recentlyRule(firstDose, 2, UpToDate)
    val rule15: Rule = doseCountRule(doses, 1, Incomplete) // else
    // 2 doses, > 5 years of age, first dose ater 12 months old,
    // second dose more than 12 months after first dose.
    val rule21: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 5 * 12) && doseAfterRule(dob, firstDose, 12) && doseAfterDoseRule(firstDose, secondDose, 24, Complete)
    // 2 doses, > 5 years of age, first dose received prior to 12 months of age and second
    // received less than two months ago.
    val rule22: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 5 * 12) && doseBeforeRule(dob, firstDose, 12) && recentlyRule(secondDose, 2, UpToDate)
    // 2 doses, > 5 years of age, first dose received prior to 12 months of age and second
    // dose received two or more months ago.
    val rule23: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 5 * 12) && doseBeforeRule(dob, firstDose, 12) && !recentlyRule(secondDose, 2, Incomplete)
    // 2 doses, > 5 years of age, second dose given fewer than 24 days after the first
    // dose and less than two months ago.
    val rule24: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 5 * 12) && !doseAfterDoseRule(firstDose, secondDose, 24) && !recentlyRule(secondDose, 2, UpToDate)
    // 2 doses, > 5 years of age, second dose given fewer than 24 days
    // after first dose and two or more months ago.
    val rule25: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 5 * 12) && !doseAfterDoseRule(firstDose, secondDose, 24) && recentlyRule(secondDose, 2, Incomplete)
    // 2 doses, > 5 years old, second dose given fewer than 24 days after
    // first dose and two or more months ago.
    val rule26: Rule = doseCountRule(doses, 2) && !youngerThanRule(dob, 5 * 12) && !doseAfterDoseRule(firstDose, secondDose, 24) && recentlyRule(secondDose, 2, Incomplete)
    val rule27: Rule = doseCountRule(doses, 2) && olderThanRule(dob, 5 * 12, Incomplete)
    val rule31: Rule = doseCountRule(doses, 3, Complete)
    val rules: Rules = new Rules(
    rules = List(rule01, rule02, rule11, rule12, rule13, rule14, rule15,
      rule21, rule22, rule23, rule24, rule25, rule26, rule31))
    val decision: RulesResult = rules.documentedDecision()
    println("MMR Decision: " + outStatus(decision.finalStatus()))
    logger.info("MMR Decision: " + outStatus(decision.finalStatus()))
    return decision
  }
}

// Vaccination status rules for HEPA (Hepatitis A)
class HEPA (dob: DateTime, doses: DateMap)
    extends Vaccine("hepA", dob, doses, 4) with SpecificRules {
  private val logger = LoggerFactory.getLogger(classOf[HEPA])
  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class HEPA immunization Status ----------\n\n")
    // zero doses, child is under 18 months old
    val rule01: Rule = doseCountRule(doses, 0) && youngerThanRule(dob, 18, UpToDate)
    val rule02: Rule = doseCountRule(doses, 0) && !youngerThanRule(dob, 18, UpToDate)
    // 1 dose, first dose received after 12 months of age and less than 12 months ago.
    val rule11: Rule = doseCountRule(doses, 1) && doseAfterRule(dob, firstDose, 12) && recentlyRule(firstDose, 12, UpToDate)
    // 1 dose, first dose received after 12 months of age and more that 12 months ago.
    val rule12: Rule = doseCountRule(doses, 1) && doseAfterRule(dob, firstDose, 12) && !recentlyRule(firstDose, 12, Incomplete)
    // 1 dose,  first dose prior to 12 months of age and
    // child is less that 18 months of age.
    val rule13: Rule = doseCountRule(doses, 1) && doseBeforeRule(dob, firstDose, 12) && youngerThanRule(dob, 18, UpToDate)
    // 1 dose, first dose recieved prior to 12 months of age and
    // child is 18 months or older.
    val rule14: Rule = doseCountRule(doses, 1, Incomplete) // else
    // 2 doses, both doses received after 12 months of age.
    val rule21: Rule = doseCountRule(doses, 2) && doseAfterRule(dob, firstDose, 12) && doseAfterRule(dob, secondDose, 12, Complete)
    // 2 doses, first dose received prior to 12 months of age and dose 2
    // received less than 2 months ago.
    val rule22: Rule = doseCountRule(doses, 2) && doseBeforeRule(dob, firstDose, 12) && recentlyRule(secondDose, 12, UpToDate)
    // 2 doses, first dose received priot to 12 months of age and dose 2
    // received 12 months of more ago.
    val rule31: Rule = doseCountRule(doses, 3, Complete)
    val rule41: Rule = doseCountRule(doses, 4, Complete)

    val rules: Rules = new Rules(
      rules = List(rule01, rule02, rule11, rule12, rule13, rule14,
        rule21, rule22, rule31, rule41))
    val decision: RulesResult = rules.documentedDecision()
    println("HEPA Decision: " + outStatus(decision.finalStatus()))
    logger.info("HEPA Decision: " + outStatus(decision.finalStatus()))
    return decision
  }
}

// Vaccination status rules for HEPB (Hepatitis B)
class HEPB (dob: DateTime, doses: DateMap)
    extends Vaccine("hepB", dob, doses, 3) with SpecificRules {
  private val logger = LoggerFactory.getLogger(classOf[HEPB])
  override def immunizationStatus (): RulesResult = {
    logger.info("---------- Class HEPB immunization Status ----------\n\n")
    // 0 doses, child is under 2 months old.
    val rule01: Rule = doseCountRule(doses, 0) && youngerThanRule(dob, 2, UpToDate)
    // 0 doses, child is over 2 months old.
    val rule02: Rule = doseCountRule(doses, 0) && !youngerThanRule(dob, 2, Incomplete)
    // 1 dose, first dose at or after 11 years old and less than 6 months ago.
    val rule11: Rule = doseCountRule(doses, 1) && olderThanRule(dob, 11 * 12) && recentlyRule(firstDose, 6, UpToDate)
    // 1 dose, first does at or after 11 years of age and 6 months or more ago.
    val rule12: Rule = doseCountRule(doses, 1) && olderThanRule(dob, 11 * 12) && !recentlyRule(firstDose, 6, Incomplete)
    // 1 dose, before 11 years of age and less than 2 months ago or
    // child is less than 4 months old.
    val rule13: Rule = doseCountRule(doses, 1) && doseBeforeRule(dob, firstDose, 11 * 12) &&
             recentlyRule(firstDose, 2) || youngerThanRule(dob, 4, UpToDate)
    // 1 dose, received before 11 years of age and more than 2 months ago
    // and child is 4 months old or older.
    val rule14: Rule = doseCountRule(doses, 1) && doseBeforeRule(dob, firstDose, 11 * 12) && !recentlyRule(firstDose, 2) && olderThanRule(dob, 4, Incomplete)
    val rule15: Rule = doseCountRule(doses, 1, Incomplete)
    // 2 doses, first dose received at or after 11 years old and second dose
    // received at least 4 months after first does.
    val rule21: Rule = doseCountRule(doses, 2) && doseBeforeRule(dob, firstDose, 11 * 12) && doseAfterDoseRule(firstDose, secondDose, 4, Complete)
    // 2 doses, first dose is received at or after 18 years old.
    val rule22: Rule = doseCountRule(doses, 2) && doseAfterRule(dob, firstDose, 18 * 12, Complete)
    // 2 doses, second dose received less that 5 months ago
    val rule23: Rule = doseCountRule(doses, 2) && recentlyRule(secondDose, 5, UpToDate)
    // 2 doses, second dose after 5 months or more ago and child is less than 18 months old.
    val rule24: Rule = doseCountRule(doses, 2) && !recentlyRule(secondDose, 5) && youngerThanRule(dob, 18, UpToDate)
    val rule31: Rule = doseCountRule(doses, 3, Complete)
    val rule41: Rule = doseCountRule(doses, 4, Complete)
    val rules: Rules = new Rules(
      rules = List(rule01, rule02, rule11, rule12, rule13, rule14, rule15,
        rule21, rule22, rule23, rule24, rule31, rule41))
    val decision: RulesResult = rules.documentedDecision()
    println("HEPB Decision: " + outStatus(decision.finalStatus()))
    logger.info("HEPB Decision: " + outStatus(decision.finalStatus()))
    return decision
    }
}

class Student(jsonMap: JsonMap){
  private val logger = LoggerFactory.getLogger(classOf[Student])
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

  def gatherStatus(): List[RulesResult] = {
    val dtapShots: DateMap = convertDateStrings(filterShots(jsonMap, "dtap.*"))
    val polioShots: DateMap = convertDateStrings(filterShots(jsonMap, "polio.*"))
    val mmrShots: DateMap = convertDateStrings(filterShots(jsonMap, "mmr.*"))
    val varicellaShots: DateMap = convertDateStrings(filterShots(jsonMap, "varicella.*"))
    val hibShots: DateMap = convertDateStrings(filterShots(jsonMap, "hib.*"))
    val hepaShots: DateMap = convertDateStrings(filterShots(jsonMap, "hepA.*"))
    val hepbShots: DateMap = convertDateStrings(filterShots(jsonMap, "hepB.*"))
    // Validate the shots.
    val dtap: DTAP = new DTAP(fullName, dob, dtapShots)
    val dtapResult: RulesResult = dtap.immunizationStatus()
    println("\nDTAP Status with " + dtapShots.size + " shots for " + fullName + " is " + dtapResult.out() + "\n")
    val hib: HIB = new HIB(dob, hibShots)
    val hibResult: RulesResult = hib.immunizationStatus()
    println("\nHIB Status with " + hibShots.size + " shots for " + fullName + " is " + hibResult.out() + "\n")
    val polio: Polio = new Polio(dob, polioShots)
    val polioResult: RulesResult = polio.immunizationStatus()
    println("\nPolio Status with " + polioShots.size + " shots for " + fullName + " is " + polioResult.out() + "\n")
    val varicella: Varicella = new Varicella(dob, List(), varicellaShots)
    val varicellaResult: RulesResult = varicella.immunizationStatus()
    println("\nvaricella Status for " + fullName + " is " + varicellaResult.out() + "\n")
    val mmr: MMR = new MMR(dob, mmrShots)
    val mmrResult: RulesResult = mmr.immunizationStatus()
    println("\nMMR Status for " + fullName + " is " + mmrResult.out() + "\n")
    val hepa: HEPA = new HEPA(dob, hepaShots)
    val hepaResult: RulesResult = hepa.immunizationStatus()
    println("\nHEPA Status for " + fullName + " is " + hepaResult.out() + "\n")
    val hepb: HEPB = new HEPB(dob, hepbShots)
    val hepbResult: RulesResult = hepb.immunizationStatus()
    println("\nHEPB Status for " + fullName + " is " + hepbResult.out() + "\n")

    return List(dtapResult,
      hibResult, polioResult, varicellaResult, mmrResult, hepaResult, hepbResult)
  }

  def printStudent():Unit = {
    val dtapShots: DateMap = convertDateStrings(filterShots(jsonMap, "dtap.*"))
    val polioShots: DateMap = convertDateStrings(filterShots(jsonMap, "polio.*"))
    val mmrShots: DateMap = convertDateStrings(filterShots(jsonMap, "mmr.*"))
    val varicellaShots: DateMap = convertDateStrings(filterShots(jsonMap, "varicella.*"))
    val hibShots: DateMap = convertDateStrings(filterShots(jsonMap, "hib.*"))
    val hepaShots: DateMap = convertDateStrings(filterShots(jsonMap, "hepA.*"))
    val hepbShots: DateMap = convertDateStrings(filterShots(jsonMap, "hepB.*"))
    logger.info("Name: " + fullName + "\tDOB: " + dob)
    logger.info("\nDTAP Shots(" + dtapShots.size + "): " + dtapShots)
    logger.info("\nPolio Shots: \t" + polioShots)
    logger.info("\nMMR Shots: \t" + mmrShots)
    logger.info("Varicella Shots: \t" + varicellaShots)
    logger.info("HIB Shots: \t" + hibShots)
    logger.info("Hepatitus A Shots: \t" + hepaShots)
    logger.info("Hepatitus B Shots: \t" + hepbShots)
  }
}

class Students (students: JsonMaps) {
  def processOneStudent(studentJson: JsonMap): (String, VaccineStatuses) = {
    val student: Student = new Student(studentJson)
    student.printStudent()
    val results:List[RulesResult] = student.gatherStatus()
    val summary: VaccineStatuses = summaryStatus(results.map(_.finalStatus))
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

    val logger = LoggerFactory.getLogger("Main")
    logger.info("\n\n\n-------------------- " + DateTime.now() + "-------------------\n\n\n\n\n")

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
    logger.debug("\n------ sampleJson > " + sampleJson)
    val sampleMap: JsonMaps = sampleAst.convertTo[JsonMaps]
    logger.debug("\n------ sampleMap > " + sampleMap)
    val student1: JsonMap = sampleMap(0)
    logger.debug("\n------ student1 > " + student1)
    val sampleStudents = new Students(sampleMap)
    logger.debug("\n------ sampleStudents\n")
    val processedStudents : List[(String, VaccineStatuses)] =
      sampleStudents.processStudents(sampleMap)
    def outProcessedStudent(p: (String, VaccineStatuses)) = (p._1, outStatus(p._2))
    logger.debug("\n------ processedStudents > " + processedStudents.map(p => outProcessedStudent(p)))

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
