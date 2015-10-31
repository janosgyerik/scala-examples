package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SafeCrackerTest extends FunSuite {
  import com.janosgyerik.scala.practice.SafeCracker._

  test("cracker invalid if no symbols") {
    intercept[IllegalArgumentException](genCrackerString("", 1))
  }

  test("cracker invalid if code length < 1") {
    intercept[IllegalArgumentException](genCrackerString("123", 0))
  }

  test("cracker 123, len=1") {
    assert("123" == genCrackerString("123", 1))
  }

  test("cracker 1, len=3") {
    assert("111" == genCrackerString("1", 3))
  }

  test("cracker 12, len=2") {
    assert("112122" == genCrackerString("12", 2))
  }

  test("cracker 12, len=3") {
    assert("111211221222" == genCrackerString("12", 3))
  }

//  test("cracker 0123456789, len=4") {
//    assert(10294 == genCrackerString("0123456789", 4).length)
//  }

  test("cracker 0123, len=3") {
    assert("000100200301101201302102202303103203311121131221231321332223233300" == genCrackerString("0123", 3))
//    assert("000100200301101201302102202303103203311121131221231321332223233300" == genCrackerString("0123", 3))
//    assert("00010020030011012013011121021131031122022123023121320321330331322232332333" == genCrackerString("0123", 3))
//    assert("00010020030011012013011121021131031122022123023121320321330331322232332333" == genCrackerString("0123", 3))
//    assert(74 == genCrackerString("0123", 3).length)
  }

  test("getNth 0123456789abcdef, 255") {
    assert("ff" == getNth("0123456789abcdef", 255))
  }
}
