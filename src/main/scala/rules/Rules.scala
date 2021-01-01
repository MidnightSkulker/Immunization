package rules

import models.model._
import org.joda.time.DateTime

// Factors that influence an immunization status.
class Factors(
  vaccineName: String = "NA",
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
  // Compare two factors, and select the last one that is not null
  // (if either are not null)
  def nonNull(x: Int, y: Int): Int = if (y != 0) y else x // 0 is the null value
  def nonNull(x: String, y: String): String = if (y != null) y else x
  def nonNull(x: DateTime, y: DateTime): DateTime = if (y != null) y else x
  def nonNull(x: List[String], y: List[String]) = if (y != null) y else x
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

case class RuleResult(description: String, factors: Factors, status: VaccineStatuses)

// A single rule such as
// "0 doses and child is less than two months old" ==> UpToDate
//
// The rule set considers the factors of a rule and renders a decision,
// where the decision is a Vaccine Status, such as UpToDate.
class Rule(
  description: String,
  factors: Factors,
  condition: Function1[Factors, Boolean],
  status: VaccineStatuses) {

  def factors(): Factors = factors
  def cond(f: Factors) = condition(f)
  def description(): String = description
  def status(): VaccineStatuses = status
  def &&(rb: Rule): Rule = {
    val combinedDescription = this.description + " and " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond(f) && rb.cond(f)
    val combinedFactors = this.factors ++ rb.factors // union of the two sets of factors
    new Rule(combinedDescription, combinedFactors, combinedCondition, rb.status)
  }
  def ||(rb: Rule): Rule = {
    val combinedDescription = this.description + " or " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond(f) || rb.cond(f)
    val combinedFactors = this.factors ++ rb.factors // union of the two sets of factors
    new Rule(combinedDescription, combinedFactors, combinedCondition, rb.status)
  }
  def unary_!(): Rule = {
    val combinedDescription = "not " + this.description
    def combinedCondition(f: Factors): Boolean = !this.cond(f)
    new Rule(combinedDescription, factors, combinedCondition, this.status)
  }
  def condStatus(f: Factors): VaccineStatuses = if (condition(factors)) status else NA
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
  def doseCountRule(numberOfDoses: Int, status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Number of doses is $numberOfDoses",
      new Factors(numberOfDoses = numberOfDoses),
      factors => factors.numberOfDoses == numberOfDoses,
      status)
  def olderThanRule(dob: DateTime, ageMonth: Int, status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Child is older than $ageMonth",
      new Factors(dob = dob, ageMonth = ageMonth),
      factors => dob.plusMonths(factors.ageMonth).isBefore(DateTime.now()),
      status)
  def youngerThanRule(
    dob: DateTime,
    ageMonth: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Child is older than $ageMonth",
      new Factors(dob = dob, ageMonth = ageMonth),
      factors => dob.plusMonths(factors.ageMonth).isAfter(DateTime.now()),
      status)
  def recentlyRule(dose: DateTime, recentMonth: Int, status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Dose is given less than $recentMonth ago",
      new Factors(dose1 = dose, recentMonth = recentMonth),
      factors => DateTime.now().plusMonths(-recentMonth).isBefore(dose),
      status)
  def newBornRule(
    dob: DateTime,
    status: VaccineStatuses = NA): Rule = youngerThanRule(dob, 2, status)
  def withinAgeRangeRule(
    dob: DateTime,
    dose: DateTime,
    startMonth: Int,
    endMonth: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Dose is given between $startMonth and $endMonth months of age",
      new Factors(dob = dob, dose1 = dose, startMonth = startMonth, endMonth = endMonth),
      factors => dose.isAfter(factors.dob.plusMonths(factors.startMonth)) &&
        (dose.isBefore(factors.dob.plusMonths(factors.endMonth))),
      status)
  def doseAfterRule(
    dob: DateTime,
    dose: DateTime,
    ageMonth: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Dose is given after age $ageMonth months of age",
      new Factors(dob = dob, dose1 = dose, ageMonth = ageMonth),
      factors => dose.isAfter(dob.plusMonths(factors.ageMonth)),
      status)
  def doseBeforeRule(
    dob: DateTime,
    dose: DateTime,
    ageMonth: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Dose is given before age $ageMonth months of age",
      new Factors(dob = dob, dose1 = dose, ageMonth = ageMonth),
      factors => dose.isBefore(dob.plusMonths(factors.ageMonth)),
      status)
  def doseAfterDoseRule(
    dose1: DateTime,
    dose2: DateTime,
    days: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Dose is given $days after another dose",
      new Factors(dose1 = dose1, dose2 = dose2, days = days),
      factors => dose2.isAfter(dose1.plusDays(days)),
      status)
  def doseBeforeDoseRule(
    dose1: DateTime,
    dose2: DateTime,
    days: Int,
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Dose is given $days before another dose",
      new Factors(dose1 = dose1, dose2 = dose2, days = days),
      factors => dose2.isBefore(dose1.plusDays(days)),
      status)
  def diseaseHistoryRule(
    disease: String,
    diseaseHistory: List[String],
    status: VaccineStatuses = NA): Rule =
    new Rule(
      s"Is $disease in disease history",
      new Factors(history = diseaseHistory),
      factors => factors.history contains disease,
      status)

}
