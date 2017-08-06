package ddc

object FrodeCalendar {
  def Frode(d: Date) : String =
    DayCard.show(d) + WeekCard.show(d) + MonthCard.show(d) + YearCard.show(d)

  def FrodeComplete(d: Date) : String =
    "Dia " + DayCard.showComplete(d) +
    ", semana " + WeekCard.showComplete(d) +
    ", mes " + MonthCard.showComplete(d) +
    ", ano " + YearCard.showComplete(d)

  private def FrodeDayFix(d: Int) : Int = {
    if (d > 60) d - 60 else d + 305
  }

  def ConvertDayCounter(d: Date) : Either[String, Int] = {
    val day = CountNumberOfDays(d)
    day match {
      case Left(a) => Left("Error in convert day counter >> ${a}")
      case Right(a) => {
        if (Date.IsLeapYear(d.year)) Right(FrodeDayFix(a + 1))
        Right(FrodeDayFix(a))
      }
    }
  }

  private def CountNumberOfDays(d: Date): Either[String, Int] = {
    if (!d.IsValid)
      Left("\nError invalid date >>")
    val leap = Date.IsLeapYearInt(d.year)
    def CountByMonth(m: Int) : Either[String, Int] = m match {
      case 1 => Right(d.day)
      case 2 => Right(d.day + 31)
      case 3 => Right(d.day + 59 + leap)
      case 4 => Right(d.day + 90 + leap)
      case 5 => Right(d.day + 120 + leap)
      case 6 => Right(d.day + 151 + leap)
      case 7 => Right(d.day + 181 + leap)
      case 8 => Right(d.day + 212 + leap)
      case 9 => Right(d.day + 243 + leap)
      case 10 => Right(d.day + 273 + leap)
      case 11 => Right(d.day + 304 + leap)
      case 12 => Right(d.day + 334 + leap)
      case _ => Left("\nError counting days by month >>")
    }
    CountByMonth(d.month) match {
      case Left(a) => Left("Error in convert numbers of days >> ${a}")
      case Right(a) => Right(a)
    }
  }
}
