package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CollectionUtilsTest extends FunSuite {
  def permutations(list: List[Int]): List[List[Int]] = {
    if (list.size < 2) List(list)
    else {
      {
        for {
          i <- (0 until list.size).toList
          xs <- permutations(list.take(i) ++ list.drop(i + 1))
        } yield list(i) :: xs
      }.distinct
    }
  }

  test("permutations of 1") {
    assert(List(List(1)) == permutations((1 to 1).toList))
  }

  test("permutations of 1 1") {
    assert(List(List(1, 1)) == permutations(List(1, 1)))
  }

  test("permutations of 1 2") {
    assert(List(List(1, 2), List(2, 1)) == permutations((1 to 2).toList))
  }

  test("permutations of 1 2 3") {
    assert(List(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1)) ==
      permutations((1 to 3).toList))
  }

  test("permutations of 1 2 3 4 size = 24") {
    assert(24 == permutations((1 to 4).toList).size)
  }

  test("permutations of 1 2 3 4 5 size = 120") {
    assert(120 == permutations((1 to 5).toList).size)
  }

  test("permutations of 1 .. 8 size = 40320") {
    assert(40320 == permutations((1 to 8).toList).size)
  }
}
