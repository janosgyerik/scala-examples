package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MergeTimelinesTest extends FunSuite {

  case class Time(hours: Int, minutes: Int) {
    def <=(other: Time) =
      hours < other.hours || hours == other.hours && minutes < other.minutes

    def +(minutes: Int) = {
      val newMinutes = this.minutes + minutes
      Time(hours + newMinutes / 60, newMinutes % 60)
    }

    override def toString: String = f"$hours%02d:$minutes%02d"
  }

  test("1:51 == 1:49 + 2") {
    assert(Time(1, 51) == Time(1, 49) + 2)
  }


  test("1:51 == 1:49 + 11") {
    assert(Time(2, 0) == Time(1, 49) + 11)
  }

  test("1:51 == 1:49 + 22") {
    assert(Time(2, 11) == Time(1, 49) + 22)
  }

  test("1:51 == 1:49 + 122") {
    assert(Time(3, 51) == Time(1, 49) + 122)
  }

  test("1:5 string = 01:05") {
    assert("01:05" == Time(1, 5).toString)
  }

  def generateTimes(start: Time, end: Time, period: Int): List[Time] = {
    if (start <= end) start :: generateTimes(start + period, end, period)
    else Nil
  }

  test("generateTimes 7:00, 9:00, 14") {
    assert("List(07:00, 07:14, 07:28, 07:42, 07:56, 08:10, 08:24, 08:38, 08:52)"
      == generateTimes(Time(7, 0), Time(9, 0), 14).toString)
  }

  case class Line(name: String)

  case class Station(name: String, line: Line)

  class Schedule(val station: Station, val times: List[Time])

  val schedule1 = new Schedule(Station("Passy", Line("M2")),
    generateTimes(Time(7, 0), Time(9, 0), 14))

  val schedule2 = new Schedule(Station("Passy", Line("M7")),
    generateTimes(Time(7, 0), Time(9, 0), 18))

  val schedule3 = new Schedule(Station("Passy", Line("M9")),
    generateTimes(Time(8, 0), Time(10, 0), 21))


}
