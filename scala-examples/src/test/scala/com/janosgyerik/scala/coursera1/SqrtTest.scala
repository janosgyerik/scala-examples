package com.janosgyerik.scala.coursera1

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SqrtTest extends FunSuite {
  import com.janosgyerik.scala.coursera1.Sqrt.sqrt

  test("square root of 4 is 2") {
    assert(2 == sqrt(4))
  }

  test("square root of 9 is 3") {
    assert(3 == sqrt(9))
  }

  def assertMaxDifference(x: Double, diff: Double): Unit = {
    assert(Math.abs(sqrt(x) - Math.sqrt(x)) < diff)
  }

  test("less than 0.1 difference") {
    assertMaxDifference(2, 0.1)
  }

  test("less than 0.01 difference") {
    assertMaxDifference(2, 0.01)
  }

  test("requires positive arg") {
    intercept[IllegalArgumentException](sqrt(-1))
  }
}
