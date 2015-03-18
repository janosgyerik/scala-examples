package com.janosgyerik.scala.practice

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SquareSumTest extends FunSuite {
  import com.janosgyerik.scala.practice.SquareSum._

  test("min squares that sum to 1") {
    assert(List(1) == minSquaresSummingTo(1))
  }

  test("min squares that sum to 4") {
    assert(List(4) == minSquaresSummingTo(4))
  }

  test("min squares that sum to 11 = 1 + 1 + 9") {
    assert(List(1, 1, 9) == minSquaresSummingTo(11))
  }

  test("min squares that sum to 12 = 4 + 4 + 4") {
    assert(List(4, 4, 4) == minSquaresSummingTo(12))
  }

  test("min squares that sum to 13 = 4 + 9") {
    assert(List(4, 9) == minSquaresSummingTo(13))
  }

  test("min squares that sum to 35") {
    assert(List(1, 16, 16) == minSquaresSummingTo(33))
  }

  test("min squares that sum to 40") {
    assert(List(4, 36) == minSquaresSummingTo(40))
  }

//  TODO: very slow!
//  test("min squares that sum to 49") {
//    assert(List(49) == minSquaresSummingTo(49))
//  }
}
