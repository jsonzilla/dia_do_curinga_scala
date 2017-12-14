package test.ddc

import ddc.Date

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class DateFixedTests extends FunSuite with BeforeAndAfter {
 var date: Date = _

  before {
    date = new Date(2018,12,21)
  }

  test("year is 2018") {
    assert(date.year == 2018)
  }

  test("month is 12") {
    assert(date.month == 12)
  }

  test("day is 12") {
    assert(date.day == 21)
  }

  test("IsValid") {
    assert(date.IsValid == true)
  }

  test("IsValidByMonth") {
    assert(date.IsValidByMonth == true)
  }

  test("Is leap year") {
    assert(Date.IsLeapYear(2012) == true)
  }

  test("Is not leap year") {
    assert(Date.IsLeapYear(2011) == false)
  }

    test("Is leap year int") {
    assert(Date.IsLeapYearInt(2012) == 1)
  }

  test("Is not leap year int") {
    assert(Date.IsLeapYearInt(2011) == 0)
  }
}

class DateTestsValid extends FunSuite {
  test("valid") {
    val date = new Date(1999,10,1)
    assert(date.IsValid == true)
  }

  test("day 0") {
    val date = new Date(1999,10,0)
    assert(date.IsValid == false)
  }
  
  test("day 32") {
    val date = new Date(1999,10,32)
    assert(date.IsValid == false)
  }

  test("29 feb leap day") {
    val date = new Date(2012,2,29)
    assert(date.IsValid == true)
  }

  test("29 feb not leap day") {
    val date = new Date(2011,2,29)
    assert(date.IsValid == false)
  }

  test("month 0") {
    val date = new Date(2011,0,29)
    assert(date.IsValid == false)
  }

  test("month 13") {
    val date = new Date(2011,13,29)
    assert(date.IsValid == false)
  }

  test("year 0") {
    val date = new Date(0,12,1)
    assert(date.IsValid == false)
  }

  test("year 1 frode") {
    val date = new Date(1790,12,1)
    assert(date.IsValid == true)
  }

  test("year 9999") {
    val date = new Date(9999,12,1)
    assert(date.IsValid == true)
  }
}