package ddc

class CardNumber(i: Int) {
  private def numerals(i: Int): Option[String] = {
    if (i < 1 || i > 14) None else Some(i.toString)
  }

  def shortFormat: Option[String] = i match {
    case 11 => Some("J")
    case 12 => Some("Q")
    case 13 => Some("K")
    case 14 => Some("Jo")
    case _ => numerals(i)
  }

  def longFormat: Option[String] = i match {
    case 1 => Some("de As")
    case 2 => Some("de Dois")
    case 3 => Some("de Tres")
    case 4 => Some("de Quatro")
    case 5 => Some("de Cinco")
    case 6 => Some("de Seis")
    case 7 => Some("de Sete")
    case 8 => Some("de Sete")
    case 9 => Some("de Nove")
    case 10 => Some("de Dez")
    case 11 => Some("de Valete")
    case 12 => Some("de Dama")
    case 13 => Some("de Rei")
    case 14 => Some("do Curinga")
    case _ => None
  }
}

class Suit(i: Int) {
  def shortFormat: Option[String] = i match {
    case 1 => Some("O")
    case 2 => Some("P")
    case 3 => Some("C")
    case 4 => Some("E")
    case _ => None
  }

  def longFormat: Option[String] = i match {
    case 1 => Some(" de ouros")
    case 2 => Some(" de paus")
    case 3 => Some(" de copas")
    case 4 => Some(" de espadas")
    case _ => None
  }
}

class Card(cn: CardNumber, s: Suit) {
  val cardNumber = cn
  val suit = s

  private def CardPrint(p: (Option[String], Option[String])): String = {
    (p._1, p._2) match {
      case (Some(a), Some(b)) => a + b
      case (Some(a), None) => "\nError in card suit >>"
      case (None, Some(a)) => "\nError in card number >>"
      case _ => "\nError in card >>"
    }
  }

  override def toString: String =
    CardPrint(cardNumber.shortFormat, suit.shortFormat)

  def toStringLongFormat: String =
    CardPrint(cardNumber.longFormat, suit.longFormat)
}
