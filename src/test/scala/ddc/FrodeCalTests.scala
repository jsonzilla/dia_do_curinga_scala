package test.ddc

import ddc.Date
import ddc.FrodeCal

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

  test("In date 2012.12.31 should be count 366 days") {
    val date = new Date(2012,12,31)
    assert(FrodeCal.CountNumberOfDays(date) == Right(366))
  }

  test("Gregorian day 200 should be Frode 140 day") {
    assert(FrodeCal.FrodeDayFix(200) == 140)
  }
  
  test("Gregorian day 59 should be Frode 364 day") {
    assert(FrodeCal.FrodeDayFix(59) == 364)
  }
  
  test("Gregorian day 61 should be Frode 1 day") {
    assert(FrodeCal.FrodeDayFix(61) == 1)
  }

  test("Gregorian day 60 should be Frode 365 day") {
    assert(FrodeCal.FrodeDayFix(60) == 365)
  }
}