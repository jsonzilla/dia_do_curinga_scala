
import ddc.Date
import ddc.FrodeCalendar

object Main extends App {
  println("\nEntre com dia mes e ano (separados por espaco):")

  val input: _root_.scala.Predef.String = scala.io.StdIn.readLine()
  val lines: Array[String] = input.split(" ")

  if (lines.length.==(3)) {
    val d = new Date(augmentString(lines.apply(2)).toInt,
                     augmentString(lines.apply(1)).toInt,
                     augmentString(lines.apply(0)).toInt)

    println(FrodeCalendar.FrodeComplete(d));
    println(FrodeCalendar.Frode(d));
  }
  else {
    println("\nDigitou errado")
  }
}
