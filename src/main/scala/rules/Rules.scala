package rules

import models.model._
import org.joda.time.DateTime

// Factors that influence an immunization status.
class Factors(vaccineName: String = "NA", numberOfDoses: Int, dob: DateTime, dose1: DateTime = null, dose2: DateTime = null, recentMonth: Int = 0, ageMonth: Int = 0, description: String = "", history: Function1[String, Boolean] = Function.const(false)) {
  def vaccineName(): String = vaccineName
  def numberOfDoses(): Int = numberOfDoses
  def dob(): DateTime = dob
  def dose1(): DateTime = dose1
  def dose2(): DateTime = dose2
  def recentMonth(): Int = recentMonth
  def ageMonth(): Int = ageMonth
  def history(disease: String): Boolean = false
}

// To be moved to another file later
class YoungerFactors(numberOfDoses: Int, dob: DateTime, ageMonth: Int)
    extends Factors(numberOfDoses = numberOfDoses,
      vaccineName = "NA",
      dob = dob,
      ageMonth = ageMonth) {
}

class RuleBit(description: String, factors: Factors, condition: Function1[Factors, Boolean]) {
  val cond: Function1[Factors, Boolean] = condition
  def description(): String = description
  def &&(rb: RuleBit): RuleBit = {
    val combinedDescription = this.description + " and " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond(f) && rb.cond(f)
    new RuleBit(combinedDescription, factors, combinedCondition)
  }
  def ||(rb: RuleBit): RuleBit = {
    val combinedDescription = this.description + " or " + rb.description()
    def combinedCondition(f: Factors): Boolean = this.cond(f) || rb.cond(f)
    new RuleBit(combinedDescription, factors, combinedCondition)
  }
  def !(): RuleBit = {
    val combinedDescription = "not " + this.description
    def combinedCondition(f: Factors): Boolean = !this.cond(f)
    new RuleBit(combinedDescription, factors, combinedCondition)
  }
}

case class RuleResult(description: String, factors: Factors, status: VaccineStatuses)
case class RulesResult(finalStatus: VaccineStatuses, report: String, factors: Factors, results: List[RuleResult])

// A single rule such as
// "0 doses and child is less than two months old" ==> UpToDate
//
// The rule set considers the factors of a rule and renders a decision,
// where the decision is a Vaccine Status, such as UpToDate.
class Rule(factors: Factors, ruleBit: RuleBit, status: VaccineStatuses) {
  def condition(f: Factors): VaccineStatuses =
    if (ruleBit.cond(factors)) status else NA
  def applyRule(): RuleResult = new RuleResult(this.description(), factors, condition(factors))
  def description(): String = ruleBit.description()
}

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

class NumberOfDosesFactors(numberOfDoses: Int, dob: DateTime)
    extends Factors(numberOfDoses = numberOfDoses,
      dob = dob,
      description = s"Age is less than $numberOfDoses") { }

object TryStuff {
  def numberOfDosesFactors(n: Int): Factors =
    new NumberOfDosesFactors(
      numberOfDoses = n,
      dob = new DateTime("11/18/2015"))
  def dosesRuleBit(n: Int): RuleBit =
    new RuleBit(
      description = s"Number of Doses is $n",
      factors = numberOfDosesFactors(n),
      condition = f => f.numberOfDoses == n)
}

