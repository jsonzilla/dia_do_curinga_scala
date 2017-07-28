
import ddc.calendar.{FrodeSimples, ImprimeCalendarioFrode}

object Main extends App {
  var dia, mes, ano: Int = 0
  var numerodedia: Int = 0
  var tmp_dia, tmp_mes, tmp_ano: Int = 0
  println("Entre com dia mes e ano (separados por enter) e precione ctrl-c")
  println("\n\tEntre com dia mes e ano (separados por espaco):")

  val input: _root_.scala.Predef.String = scala.io.StdIn.readLine()
  val lines: Array[String] = input.split(" ")

  if (lines.length.==(3)) {
    ImprimeCalendarioFrode(augmentString(lines.apply(0)).toInt, augmentString(lines.apply(1)).toInt, augmentString(lines.apply(2)).toInt);
    println("\n\tSimples -- ".+(FrodeSimples(Main.dia, Main.mes, Main.ano)));
  }
  else {
    println("\nDigitou errado");
  }
}
