package chap11.scala

import chapter11.java._
import java.util.Date

class PaymentCalculator(val payPerDay: Int = 100) extends DateCalculator {
  def calculatePayment(start: Date, end: Date) = {
    daysBetween(start, end) * payPerDay
  }
}

