package c03.exception

import Either._

trait Quote

object Quote {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      ticket <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, ticket)
}
