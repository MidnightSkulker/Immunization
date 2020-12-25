package rules

import org.joda.time.DateTime

// Factors that influence an immunization status.
abstract class Factors(name: String, numberOfDoses: Int, dob: DateTime) {
  def name(): String = name
  def numberOfDoses(): Int = numberOfDoses
  def dob(): DateTime = dob
  def dose1(): DateTime = null
  def dose2(): DateTime = null
  def recentMonth(): Int = 0
  def ageMonth(): Int = 0
  def description(): String = ""
  def history(disease: String): Boolean = false
}

// A set of factors for a particular decision on the immunization status.
class FactorSet(numberOfDoses: Int,
  name: String,
  description: String,
  dob: DateTime,
  dose1: DateTime = null,
  dose2: DateTime = null,
  recentMonth: Int = 0,
  ageMonth: Int = 0,
  history: Boolean = false) extends Factors(name, numberOfDoses, dob) {

  override def name() = name
  override def dose1() = dose1
  override def dose2(): DateTime = dose2
  override def recentMonth(): Int = recentMonth
  override def ageMonth(): Int = ageMonth
  override def description(): String = description
  override def history(disease: String): Boolean = history
}

// To be moved to another file later
class YoungerFactors(numberOfDoses: Int, dob: DateTime, ageMonth: Int)
    extends FactorSet(numberOfDoses = numberOfDoses,
      name = "",
      description = "Child is younger than " + ageMonth + " months",
      dob = dob,
      dose1 = null,
      dose2 = null,
      recentMonth = 0,
      ageMonth = ageMonth,
      history = false) {
}

class RuleBit(description: String, factors: Factors, condition: Function1[Factors, Boolean]) {
  def &&(rb: RuleBit): RuleBit = new RuleBit("", factors, condition) // this.condition() && rb.condition()
}
