package rules

import models.model._
import org.joda.time.DateTime

// Factors that influence an immunization status.
class Factors(
  vaccineName: String = "NA",
  numberOfDoses: Int = 0,
  dob: DateTime,
  dose1: DateTime = null,
  dose2: DateTime = null,
  recentMonth: Int = 0,
  ageMonth: Int = 0,
  history: Function1[String, Boolean] = Function.const(false)) {
  def vaccineName(): String = vaccineName
  def numberOfDoses(): Int = numberOfDoses
  def dob(): DateTime = dob
  def dose1(): DateTime = dose1
  def dose2(): DateTime = dose2
  def recentMonth(): Int = recentMonth
  def ageMonth(): Int = ageMonth
  def history(disease: String): Boolean = false
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

  def cond(f: Factors) = condition(f)
  def description(): String = description
  def status(): VaccineStatuses = status
  def &&(rb: Rule): Rule = {
    val combinedDescription = this.description + " and " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond(f) && rb.cond(f)
    new Rule(combinedDescription, factors, combinedCondition, rb.status)
  }
  def ||(rb: Rule): Rule = {
    val combinedDescription = this.description + " or " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond(f) || rb.cond(f)
    new Rule(combinedDescription, factors, combinedCondition, rb.status)
  }
  def !(): Rule = {
    val combinedDescription = "not " + this.description
    def combinedCondition(f: Factors): Boolean = !this.cond(f)
    new Rule(combinedDescription, factors, combinedCondition, this.status)
  }
  def condStatus(f: Factors): VaccineStatuses = if (condition(factors)) status else NA
  def applyRule(): RuleResult = new RuleResult(this.description(), factors, condStatus(factors))
}

case class RulesResult(finalStatus: VaccineStatuses, report: String, factors: Factors, results: List[RuleResult])

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
      factors => dob.plusMonths(ageMonth).isBefore(DateTime.now()),
      status
    )
}
