package rules

import org.joda.time.DateTime

// Factors that influence an immunization status.
class Factors(dob: DateTime) {
  val dobv: DateTime = dob
  def dose1(): DateTime = null
  def dose2(): DateTime = null
  def recentMonth(): Int = 0
  def ageMonth(): Int = 0
  def description(): String = ""
  def history(disease: String): Boolean = false
}
