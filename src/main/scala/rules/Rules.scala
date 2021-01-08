package rules

import models.model._
import org.joda.time.DateTime

// Factors that influence an immunization status.
class Factors(
  vaccineName: String = "",
  numberOfDoses: Int = 0,
  dob: DateTime = null,
  dose1: DateTime = null,
  dose2: DateTime = null,
  recentMonth: Int = 0,
  ageMonth: Int = 0,
  startMonth: Int = 0,
  endMonth: Int = 0,
  days: Int = 0,
  history: List[String] = List()) {
  def nullFactors(): Factors = new Factors() // Null collection of factors (default all constructors).
  def vaccineName(): String = vaccineName
  def numberOfDoses(): Int = numberOfDoses
  def dob(): DateTime = dob
  def dose1(): DateTime = dose1
  def dose2(): DateTime = dose2
  def recentMonth(): Int = recentMonth
  def ageMonth(): Int = ageMonth
  def startMonth(): Int = startMonth
  def endMonth(): Int = endMonth
  def days(): Int = days
  def history(): List[String] = history
  def outFactors(): String = " Factors: # doses: " + numberOfDoses + " -- dose1: " + dose1 + " -- ageMonth: " + ageMonth ++ " -- dob: " + dob
  // Compare two factors, and select the last one that is not null
  // (if either are not null).
  def nonNull(x: Int, y: Int): Int = if (x != 0) x else y // 0 is the null value
  def nonNull(x: String, y: String): String = if (x != null) x else y
  def nonNull(x: DateTime, y: DateTime): DateTime = if (x != null) x else y
  def nonNull(x: List[String], y: List[String]) = if (x != null) x else y
  // Combine two sets of factors (union)
  def ++(f2: Factors): Factors = new Factors(
    nonNull(this.vaccineName, f2.vaccineName),
    nonNull(this.numberOfDoses, f2.numberOfDoses), // May not need this
    nonNull(this.dob, f2.dob),
    nonNull(this.dose1, f2.dose1),
    nonNull(this.dose2, f2.dose2),
    nonNull(this.recentMonth, f2.recentMonth),
    nonNull(this.ageMonth, f2.ageMonth),
    nonNull(this.startMonth, f2.startMonth),
    nonNull(this.endMonth, f2.endMonth),
    nonNull(this.days, f2.days),
    nonNull(this.history, f2.history))
  }

case class RuleResult(
  description: String,
  factors: Factors,
  status: VaccineStatuses)   // Status if applicable, NA otherwise

// A single rule such as
// "0 doses and child is less than two months old" ==> UpToDate
//
// The rule set considers the factors of a rule and renders a decision,
// where the decision is a Vaccine Status, such as UpToDate.
//
// Currently working on this:
// doseCountRule is 4, 0, false
// doseCountRule && youngerThanRule ==> UpToDate
// Should not have evaluated youngerThanRule.

class Rule(
  name: String,
  description: String,
  factors: Factors,
  condition: Function1[Factors, Boolean],
  status: VaccineStatuses) {

  def factors(): Factors = factors
  def cond() = condition(factors)
  def description(): String = description
  def name(): String = name
  def status(): VaccineStatuses = status
  def outRule(r: Rule): String = "^^^^^^ RULE: " + r.name() + " -- Description: " + r.description() + " -- status: " + outStatus(r.status()) + " -- factors " + r.factors.outFactors()
  def &&(rb: Rule): Rule = {
    val combinedName: String = this.name + " && " + rb.name
    val combinedDescription = this.description + " and " + rb.description()
    def combinedCondition(f: Factors): Boolean = {
      // Short Circuit
      if (this.cond() == false) {
        return false
      }
      else rb.cond()
    }
    val combinedFactors = this.factors ++ rb.factors // Union of the two sets of factors.
    // When this rule is false, short circuit execution of the next (rb) rule.
    val combinedStatus = if (!this.cond()) ShortCircuit else rb.status
    val ret = new Rule(
      name = combinedName,
      description = combinedDescription,
      factors = combinedFactors,
      condition = combinedCondition,
      status = combinedStatus)
    // println("Rules.ret && -- " + this.cond() + ", combinedStatus: " + outStatus(combinedStatus) + ", " + outRule(ret))
    return ret
  }
  def ||(rb: Rule): Rule = {
    val combinedName: String = this.name + " || " + rb.name
    val combinedDescription: String = this.description + " or " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond() || rb.cond()
    val combinedFactors = this.factors ++ rb.factors // union of the two sets of factors
    new Rule(combinedName, combinedDescription, combinedFactors, combinedCondition, rb.status)
  }
  def unary_!(): Rule = {
    val combinedName: String = "!" + name
    val combinedDescription = "not " + this.description
    def combinedCondition(f: Factors): Boolean = !this.cond()
    new Rule(combinedName, combinedDescription, factors, combinedCondition, this.status)
  }
  def condStatus(f: Factors): VaccineStatuses = {
    val ret = if (condition(factors)) status else NA
    println(name() + " ==> " + outStatus(status))
    return ret
  }
  def applyRule(): RuleResult = new RuleResult(this.description(), factors, condStatus(factors))
}

case class RulesResult(
  finalStatus: VaccineStatuses,
  report: String,
  factors: Factors,
  results: List[RuleResult])

// A rule set is a collection of rules.
// Each rule in the rule set considers a different case, and renders a decision.
// If none of the rules render a decision, the vaccine status will be Incomplete.
class Rules(rules: List[Rule]) {
  // Combine the sets of factors from a list of rules into a single set of factors
  def listFactors(): Factors =
    rules.foldLeft (new Factors()) {(acc, r) => acc ++ r.factors()}
  // Get a rule result for each rule in the list of rules.
  def applyRules(): List[RuleResult] = rules.map(_.applyRule)
  // Summary decision for all the rules.
  def documentedDecision(): RulesResult = {
    val results: List[RuleResult] = applyRules()
    val nonNAResults: List[RuleResult] = results.filter(r => r.status != NA)
    val (finalStatus, report) =
      nonNAResults.size match {
        case 0 => (NA, "No rule matched") // Exactly one rule should match.
        case 1 => (nonNAResults(0).status, nonNAResults(0).description)
        case default => (Error, "More than one rule matched")
      }
    return new RulesResult(finalStatus, report, this.listFactors(), results)
  }
  def report(): String = documentedDecision().report
}

trait SpecificRules {
  def errorRule(error: String, factors: Factors): Rule =
    new Rule(name = error, description = error, factors = factors, condition = factors => false, status = Error)
  def doseCountRule(doses: DateMap, numberOfDoses: Int, status: VaccineStatuses = NA): Rule =
    new Rule(
      name = "doseCountRule",
      description = s"Number of doses is $numberOfDoses",
      factors = new Factors(numberOfDoses = doses.size),
      factors => factors.numberOfDoses == numberOfDoses,
      status = status)
  def olderThanRule(dob: DateTime, ageMonth: Int, status: VaccineStatuses = NA): Rule =
    new Rule(
      name = "olderThanRule",
      description = s"Child is older than $ageMonth",
      factors = new Factors(dob = dob, ageMonth = ageMonth),
      factors => dob.plusMonths(factors.ageMonth).isBefore(DateTime.now()),
      status)
  def youngerThanRule(
    dob: DateTime,
    ageMonth: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      name = "youngerThanRule",
      description = s"Child is older than $ageMonth months",
      factors = new Factors(dob = dob, ageMonth = ageMonth),
      factors => dob.plusMonths(factors.ageMonth).isAfter(DateTime.now()),
      status = status)
  def recentlyRule(dose: Option[DateTime], recentMonth: Int, status: VaccineStatuses = NA): Rule =
    dose match {
      case Some(d) => new Rule(
        name = "recentlyRule",
        description = s"Dose is given less than $recentMonth ago",
        factors = new Factors(dose1 = d, recentMonth = recentMonth),
        factors => DateTime.now().plusMonths(-recentMonth).isBefore(d),
        status)
      case None => errorRule("None dose passed in / recentlyRule", new Factors())
    }
  def newBornRule(
    dob: DateTime,
    status: VaccineStatuses = NA): Rule = youngerThanRule(dob, 2, status)
  def withinAgeRangeRule(
    dob: DateTime,
    dose: Option[DateTime],
    startMonth: Int,
    endMonth: Int,
    status: VaccineStatuses = NA): Rule =
    dose match {
      case Some(d) => new Rule(
        name = "recentlyRule",
        description = s"Dose is given between $startMonth and $endMonth months of age",
        factors = new Factors(dob = dob, dose1 = d, startMonth = startMonth, endMonth = endMonth),
        factors => d.isAfter(factors.dob.plusMonths(factors.startMonth)) &&
        (d.isBefore(factors.dob.plusMonths(factors.endMonth))),
        status)
      case None => errorRule("None dose passed in / withinAgeRangeRule", new Factors())
    }
  def doseAfterRule(
    dob: DateTime,
    dose: Option[DateTime],
    ageMonth: Int,
    status: VaccineStatuses = NA): Rule =
    dose match {
      case Some(d) => new Rule(
        name = "doseAfterRule",
        description = s"Dose is given after age $ageMonth months of age",
        factors = new Factors(dob = dob, dose1 = d, ageMonth = ageMonth),
        factors => d.isAfter(dob.plusMonths(factors.ageMonth)),
        status = status)
      case None => errorRule("None dose passed in / doseAfterRule", new Factors())
    }
  def doseBeforeRule(
    dob: DateTime,
    dose: Option[DateTime],
    ageMonth: Int,
    status: VaccineStatuses = NA): Rule =
    dose match {
      case Some(d) => new Rule(
        name = "doseBeforeRule",
        description = s"Dose is given before age $ageMonth months of age",
        factors = new Factors(dob = dob, dose1 = d, ageMonth = ageMonth),
        factors => d.isBefore(dob.plusMonths(factors.ageMonth)),
        status = status)
      case None => errorRule("None dose passed in / doseBeforeRule", new Factors())
    }
  def doseAfterDoseRule(
    dose1: Option[DateTime],
    dose2: Option[DateTime],
    days: Int,
    status: VaccineStatuses = NA): Rule =
    dose1 match {
      case Some(d1) =>
        dose2 match {
          case Some(d2) => new Rule(
            name = "doseAfterDoseRule",
            description = s"Dose is given $days after another dose",
            factors = new Factors(dose1 = d1, dose2 = d2, days = days),
            factors => d2.isAfter(d1.plusDays(days)),
            status = status)
          case None => errorRule("None dose2 passed in / doseAfterDoseRule", new Factors())
        }
      case None => errorRule("None dose1 passed in / doseAfterDoseRule", new Factors())
    }

  def doseBeforeDoseRule(
    dose1: Option[DateTime],
    dose2: Option[DateTime],
    days: Int,
    status: VaccineStatuses = NA): Rule =
    dose1 match {
      case Some(d1) =>
        dose2 match {
          case Some(d2) => new Rule(
            name = "doseBeforeDoseRule",
            description = "Dose is given $days before another dose",
            factors = new Factors(dose1 = d1, dose2 = d2, days = days),
            factors => d2.isBefore(d1.plusDays(days)),
            status = status)
          case None => errorRule("None dose2 passed in / doseBeforeDoseRule", new Factors())
        }
      case None => errorRule("None dose1 passed in / doseBeforeDoseRule", new Factors())
    }

  def diseaseHistoryRule(
    disease: String,
    diseaseHistory: List[String],
    status: VaccineStatuses = NA): Rule =
    new Rule(
      name = "diseaseHistoryRule",
      description = s"Is $disease in disease history",
      factors = new Factors(history = diseaseHistory),
      factors => factors.history contains disease,
      status = status)

}
