package ddc {
  object calendar {
    def EhAnoBissexto(ano: Int) : Boolean = {
      (ano % 400 == 0) || (ano % 4 == 0 && ano % 100 != 0)
    }

    def CorrigeDiaFrode(dia: Int) : Int = {
      if (dia > 60)  dia - 60
      else dia + 305
    }

    def CorrigeDiaFrodeVerificaBissexto(dia: Int, ano: Int): Int = {
      if (EhAnoBissexto(ano - 1)) CorrigeDiaFrode(dia) + 1
      CorrigeDiaFrode(dia)
    }

    def CorrigeAnoFrode(ano: Int) : Int = {
      ano - 1790
    }

    def NaipeAnoFrode(ano: Int): Int = {
      (CorrigeAnoFrode(ano) / 13) % 4
    }

    def CartaAnoFrode(ano: Int): Int = {
      CorrigeAnoFrode(ano) % 13
    }

    def IntAnoBissexto(ano: Int) : Int = {
      if (EhAnoBissexto(ano)) 1
      0
    }

    def EstacoesFrode(dia: Int, ano: Int) : Int = {
      val biss = IntAnoBissexto(ano - 1)
      if (dia <= (62 - biss)) 1
      else if (dia <= (154 - biss)) 2
      else if (dia <= (247 - biss)) 3
      else if (dia <= (338 - biss)) 0
      else if (dia <= (367 - biss)) 1
      1 //HIRO tá errado
    }

    def Mes(dia: Int): Int = {
      ((dia - 1) / 28) % 13
    }

    def NaipeSemanaFrode(dia: Int) : Int = {
      ((dia / 7)/ 13) % 4
    }

    def CartaSemanaFrode(dia: Int): Int = {
      (dia / 7) % 13
    }

    def NaipeDiaFrode(dia: Int) : Int ={
      ((dia - 1) / 13) % 4; // dia decrementando por causa da natureza da array de char
    }

    def CartaDiaFrode(dia: Int): Int = {
      (dia - 1) % 13; // dia decrementando por causa da natureza da array de char
    }

    def EhDataValida(dia: Int, mes: Int, ano: Int): Boolean = {
      if ((dia < 1) | ((mes < 1)||(mes > 12)) |
        ((ano < 1790)||(ano > 9999)) |
        (((mes == 1)||(mes == 3)||(mes == 5)||(mes == 7)||(mes == 8)||(mes == 10 )||(mes == 12)) & (dia > 31)) |
        (((mes == 4)||(mes == 6)||(mes == 9)||(mes ==11)) & (dia > 30)) |
        (((mes == 2) & EhAnoBissexto(ano)) && (dia > 29)) |
        ((mes == 2) & !EhAnoBissexto(ano)) && (dia > 28)) false
      else true
    }

    def DiaDoAno(dia: Int, mes: Int, ano: Int): Int = {
      if (!EhDataValida(dia, mes, ano)) 0
      val biss = IntAnoBissexto(ano)
      def ContaPorMes(m: Int) : Int = m match {
        case 1 => dia
        case 2 => dia + 31
        case 3 => dia + 59 + biss
        case 4 => dia + 90 + biss
        case 5 => dia + 120 + biss
        case 6 => dia + 151 + biss
        case 7 => dia + 181 + biss
        case 8 => dia + 212 + biss
        case 9 => dia + 243 + biss
        case 10 => dia + 273 + biss
        case 11 => dia + 304 + biss
        case 12 => dia + 334 + biss
        case _ => 0
      }
      ContaPorMes(mes)
    }

    def FrodeSimples(dia: Int, mes: Int, ano: Int) : String = { //RECEBE DATA ORIGINAL
      val cartas = List("1","2","3","4","5","6","7","8","9","10","J","Q","K","JO")
      val naipes = List("O","P","C","E")
      val numerodedia = CorrigeDiaFrode( DiaDoAno(dia,mes,ano))

      val sDia = if(numerodedia < 365) cartas(CartaDiaFrode(numerodedia)) + naipes(NaipeDiaFrode(numerodedia))
      else  cartas(13)
      val sSemana = cartas(CartaSemanaFrode(numerodedia)) + naipes(NaipeSemanaFrode(numerodedia))
      val sMes = cartas(Mes(numerodedia)) + naipes(EstacoesFrode(dia, ano))
      val sAno = cartas(CartaAnoFrode(ano)) + naipes(NaipeAnoFrode(ano))
      sDia + sSemana + sMes + sAno
    }

    def ImprimeCalendarioFrode(dia: Int, mes: Int, ano: Int) { //RECEBE DATA ORIGINAL
      val cartas = List("de As","de Dois","de Tres","de Quatro","de Cinco",
        "de Seis","de Sete","de Oito","de Nove","de Dez",
        "de Valete","de Dama","de Rei","do Curinga")
      val naipes = List(" de ouros"," de paus"," de copas"," de espadas")

      def CuringaStringLonga(dia: Int) : String =
        dia match {
          case 365 => "\tDia " + cartas(13)
          case 366 => "\tDuplo dia " + cartas(13)
          case _ => "\tDia de " + cartas(CartaDiaFrode(dia)) + naipes(NaipeDiaFrode(dia))
        }


      val numerodedia = CorrigeDiaFrode( DiaDoAno(dia,mes,ano ))

      println("\n\tCalendario de Paciencia de Frode")
      println("\n\t---------------------------------\n")

      println(CuringaStringLonga(numerodedia));

      println("\tSemana de " + cartas(CartaSemanaFrode(numerodedia)) + naipes(NaipeSemanaFrode(numerodedia)))
      println("\tMes de " + cartas(Mes(numerodedia)) + " estacao" + naipes(EstacoesFrode(dia, ano)))
      println("\tAno de " + cartas(CartaAnoFrode(ano)) + naipes(NaipeAnoFrode(ano)))
      println("\n\t" + dia + "/" + mes + "/" + ano + " e dia numero " + numerodedia)
    }
  }
}
