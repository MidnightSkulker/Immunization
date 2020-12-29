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
  history: Function1[String, Boolean] = Function.const(false)) {
  def vaccineName(): String = vaccineName
  def numberOfDoses(): Int = numberOfDoses
  def dob(): DateTime = dob
  def dose1(): DateTime = dose1
  def dose2(): DateTime = dose2
  def recentMonth(): Int = recentMonth
  def ageMonth(): Int = ageMonth
  def startMonth(): Int = startMonth
  def endMonth(): Int = endMonth
  def history(disease: String): Boolean = false
  // Compare two factors, and select the last one that is not null
  // (if either are not null)
  def nonNull(x: Int, y: Int): Int = if (y != 0) y else x // 0 is the null value
  def nonNull(x: String, y: String): String = if (y != null) y else x
  def nonNull(x: DateTime, y: DateTime): DateTime = if (y != null) y else x
  def nonNull(x: Function1[String, Boolean], y: Function1[String, Boolean]): Function1[String, Boolean] = if (y != null) y else x
  // Combine two sets of factors
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
  def !(): Rule = {
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
class Rules(factors: Factors, rules: List[Rule]) {
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
    return new RulesResult(finalStatus, report, factors, results)
  }
  def report(): String = documentedDecision().report
}

trait SpecificRules {
  def olderThanRule(dob: DateTime, ageMonth: Int, status: VaccineStatuses): Rule =
    new Rule(
      s"Child is older than $ageMonth",
      new Factors(dob = dob, ageMonth = ageMonth),
      factors => dob.plusMonths(factors.ageMonth).isBefore(DateTime.now()),
      status)
  def youngerThanRule(dob: DateTime, ageMonth: Int, status: VaccineStatuses): Rule =
    new Rule(
      s"Child is older than $ageMonth",
      new Factors(dob = dob, ageMonth = ageMonth),
      factors => dob.plusMonths(factors.ageMonth).isAfter(DateTime.now()),
      status)
  def recentlyRule(dose: DateTime, recentMonth: Int, status: VaccineStatuses): Rule =
    new Rule(
      s"Dose is given less than $recentMonth ago",
      new Factors(dose1 = dose, recentMonth = recentMonth),
      factors => DateTime.now().plusMonths(-recentMonth).isBefore(dose),
      status)
  def newBornRule(dob: DateTime, status: VaccineStatuses): Rule = youngerThanRule(dob, 2, status)
  def withinAgeRangeRule(
    dob: DateTime,
    dose: DateTime,
    startMonth: Int,
    endMonth: Int,
    status: VaccineStatuses): Rule =
    new Rule(
      s"Dose is given between $startMonth and $endMonth months of age",
      new Factors(dob = dob, dose1 = dose, startMonth = startMonth, endMonth = endMonth),
      factors => dose.isAfter(factors.dob.plusMonths(factors.startMonth)) &&
        (dose.isBefore(factors.dob.plusMonths(factors.endMonth))),
      status)
}
