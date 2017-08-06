
import ddc.calendar.{FrodeSimples, ImprimeCalendarioFrode}

object Main extends App {
  println("Entre com dia mes e ano (separados por enter) e precione ctrl-c")
  println("\n\tEntre com dia mes e ano (separados por espaco):")

  val input: _root_.scala.Predef.String = scala.io.StdIn.readLine()
  val lines: Array[String] = input.split(" ")

  if (lines.length.==(3)) {
    ImprimeCalendarioFrode(augmentString(lines.apply(0)).toInt, augmentString(lines.apply(1)).toInt, augmentString(lines.apply(2)).toInt)
    println("\n\tSimples -- ".+(FrodeSimples(augmentString(lines.apply(0)).toInt, augmentString(lines.apply(1)).toInt, augmentString(lines.apply(2)).toInt)))
  }
  else {
    println("\nDigitou errado")
  }
}
