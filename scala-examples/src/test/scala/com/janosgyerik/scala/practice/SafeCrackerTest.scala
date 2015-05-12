package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SafeCrackerTest extends FunSuite {
  import com.janosgyerik.scala.practice.SafeCracker._

  test("cracker invalid if no symbols") {
    intercept[IllegalArgumentException](cracker("", 1))
  }

  test("cracker invalid if code length < 1") {
    intercept[IllegalArgumentException](cracker("123", 0))
  }

  test("cracker 123, len=1") {
    assert("123" == cracker("123", 1))
  }

  test("cracker 1, len=3") {
    assert("111" == cracker("1", 3))
  }

  test("cracker 12, len=2") {
    assert("112122" == cracker("12", 2))
  }

  test("cracker 12, len=3") {
    assert("111211221222" == cracker("12", 3))
  }

  test("cracker 0123456789, len=4") {
    assert(10294 == cracker("0123456789", 4).length)
  }

  test("getNth 0123456789abcdef, 255") {
    assert("ff" == getNth("0123456789abcdef", 255))
  }
}
