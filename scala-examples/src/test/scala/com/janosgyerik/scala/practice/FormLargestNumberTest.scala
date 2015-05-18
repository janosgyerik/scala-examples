package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FormLargestNumberTest extends FunSuite {

  def formLargestNumber(nums: Int*) = {
    nums.map(_.toString).sortWith((s1, s2) => (s1 + s2) > (s2 + s1)).mkString.toInt
  }

  test("50, 2, 1, 9 -> 95021") {
    assert(95021 == formLargestNumber(50, 2, 1, 9))
  }

  test("7, 91, 5 -> 9175") {
    assert(9175 == formLargestNumber(7, 91, 5))
  }

  test("3, 30, 34, 5, 9 -> 9534330") {
    assert(9534330 == formLargestNumber(3, 30, 34, 5, 9))
  }

  test("420, 42, 423 -> 42423420") {
    assert(42423420 == formLargestNumber(420, 42, 423))
  }
}
