package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class MaxNumberTest extends FunSuite {

  test("max from 2, 5, 9, 4") {
    assert(9542 == MaxNumber.solve(Seq(2, 5, 9, 4)))
  }

  test("max from 4, 98, 10, 110, 91") {
    // 98 91 4 110 10
    assert(9891411010L == MaxNumber.solve(Seq(4, 98, 10, 110, 91)))
  }
}
