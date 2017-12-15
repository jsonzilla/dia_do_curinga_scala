package test.ddc

import ddc.{Date,FrodeCal}

import org.scalatest.FunSuite

class FrodeCalTests extends FunSuite {
  test("In date 2017.17.31 should be raise a error") {
    val date = new Date(2017,17,31)
    assert(FrodeCal.CountNumberOfDays(date) == Left("Error invalid date >> "))
  }
  
  test("In date 2017.7.19 should be count 200 days") {
    val date = new Date(2017,7,19)
    assert(FrodeCal.CountNumberOfDays(date) == Right(200))
  }

  test("In date 2017.1.1 should be count 1 day") {
    val date = new Date(2017,1,1)
    assert(FrodeCal.CountNumberOfDays(date) == Right(1))
  }

  test("In date 2017.12.31 should be count 365 days") {
    val date = new Date(2017,12,31)
    assert(FrodeCal.CountNumberOfDays(date) == Right(365))
  }

  test("In date 2017.12.31 should not be count 366 days") {
    val date = new Date(2017,12,31)
    assert(FrodeCal.CountNumberOfDays(date) != Right(366))
  }

  test("Gregorian day 200 should be Frode 140 day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 200) == 140)
  }

  test("Gregorian day 366 should be Frode 140 day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 366) == 306)
  }

  test("Gregorian day 365 should be Frode 140 day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 365) == 305)
  }
  
  test("Gregorian day 59 should be Frode 364 day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 59) == 364)
  }
  
  test("Gregorian day 60 should be Frode 0 (joker) day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 60) == 0)
  }

  test("Gregorian day 61 should be Frode 1 day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 61) == 1)
  }

  test("Gregorian day 62 should be Frode 2 day") {
    val date = new Date(2012)
    assert(FrodeCal.FrodeDayFix(date, 62) == 2)
  }

  test("Date 2017.7.18 should be compact frode 9C8P6P7P") {
    val date = new Date(2017,7,18)
    assert(FrodeCal.Compact(date) == "9C7P5P7P")
  }

  test("Date 2017.7.19 should be compact frode 10C8P6P7P") {
    val date = new Date(2017,7,19)
    assert(FrodeCal.Compact(date) == "10C8P6P7P")
  }

  test("Date 2011.2.27 should be compact frode QEKEKP1P") {
    val date = new Date(2011,2,27)
    assert(FrodeCal.Compact(date) == "QEKEKP1P")
  }

  test("Date 2011.2.28 should be compact frode KE1O1P1P") {
    val date = new Date(2011,2,28)
    assert(FrodeCal.Compact(date) == "KE1O1P1P")
  }

  test("Date 2011.3.1 should be compact frode 1O1O1P1P") {
    val date = new Date(2011,3,1)
    assert(FrodeCal.Compact(date) == "1O1O1P1P")
  }

  test("Date 2012.2.27 should be compact frode QEKEKP2P") {
    val date = new Date(2012,2,27)
    assert(FrodeCal.Compact(date) == "QEKEKP2P")
  }

  test("Date 2012.2.28 should be compact frode KE1O1P2P") {
    val date = new Date(2012,2,28)
    assert(FrodeCal.Compact(date) == "KE1O1P2P")
  }

  test("Date 2012.2.29 should be compact frode Jo1O1P2P") {
    val date = new Date(2012,2,29)
    assert(FrodeCal.Compact(date) == "Jo1O1P2P")
  }

  test("Date 2012.3.1 should be compact frode 1O1O1P2P") {
    val date = new Date(2012,3,1)
    assert(FrodeCal.Compact(date) == "1O1O1P2P")
  }

  test("Date 2013.2.28 should be compact frode KE1O1P3P") {
    val date = new Date(2013,2,28)
    assert(FrodeCal.Compact(date) == "KE1O1P3P")
  }

  test("Date 2013.3.1 should be compact frode 1O1O1P3P") {
    val date = new Date(2013,3,1)
    assert(FrodeCal.Compact(date) == "1O1O1P3P")
  }

  test("Date 2014.2.28 should be compact frode KE1O1P4P") {
    val date = new Date(2014,2,28)
    assert(FrodeCal.Compact(date) == "KE1O1P4P")
  }

  test("Date 2014.3.1 should be compact frode 1O1O1P4P") {
    val date = new Date(2014,3,1)
    assert(FrodeCal.Compact(date) == "1O1O1P4P")
  }
}