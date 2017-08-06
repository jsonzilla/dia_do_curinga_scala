package ddc

trait CalendarCard {
  def card(d: Date): Either[String, Card]

  def show(d: Date): String = {
    card(d) match {
      case Right(a) => a.toString
      case Left(a) => "\nError in CalendarCard >> " + a
    }
  }
  def showComplete(d: Date): String = {
      card(d) match {
      case Right(a) => a.toStringLongFormat
      case Left(a) => "\nError in CalendarCard >> " + a
    }
  }
}

object YearCard extends CalendarCard {
  def card(d: Date) : Either[String, Card]  = {
    def suit: Int = ((d.year - 1790) / 13) % 4 + 1
    def cardNumber: Int = (d.year - 1790) % 13 + 1
    Right(new Card(new CardNumber(cardNumber), new Suit(suit)))
  }
}

object MonthCard extends CalendarCard {
  def card(d: Date) : Either[String, Card]  = {
    def suit : Int = {
      val leap = Date.IsLeapYearInt(d.year - 1)
      if (d.day <= (62 - leap)) 2
      else if (d.day <= (154 - leap)) 3
      else if (d.day <= (247 - leap)) 4
      else if (d.day <= (338 - leap)) 1
      else if (d.day <= (367 - leap)) 2
      2
    }
    def cardNumber: Int = ((d.day - 1) / 28) % 13 + 1
    Right(new Card(new CardNumber(cardNumber), new Suit(suit)))
  }
}

object WeekCard extends CalendarCard {
  def card(d: Date) : Either[String, Card] = {
    val nd = FrodeCalendar.ConvertDayCounter(d)
    nd match {
      case Left(a) => Left(a)
      case Right(ndd) => {
        def suit: Int = ((ndd / 7)/ 13) % 4 + 1
        def cardNumber: Int = (ndd / 7) % 13 + 1
        Right(new Card(new CardNumber(cardNumber), new Suit(suit)))
      }
    }
  }
}

object DayCard extends CalendarCard {
  def card(d: Date) :  Either[String, Card] = {
    val nd = FrodeCalendar.ConvertDayCounter(d)
    nd match {
      case Left(a) => Left(a)
      case Right(ndd) => {
        def suit: Int = (ndd / 13) % 4 + 1
        def cardNumber: Int = ndd % 13 + 1
        Right(new Card(new CardNumber(cardNumber), new Suit(suit)))
      }
    }
  }
}