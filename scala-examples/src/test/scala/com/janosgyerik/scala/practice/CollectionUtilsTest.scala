package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CollectionUtilsTest extends FunSuite {
  def except[A](list: List[A], index: Int) = {
    list.take(index) ++ list.drop(index + 1)
  }

  test("1 2 3 except idx=0 is 2 3") {
    assert(List(2, 3) == except(List(1, 2, 3), 0))
  }

  test("1 2 3 except idx=1 is 1 3") {
    assert(List(1, 3) == except(List(1, 2, 3), 1))
  }

  test("1 2 3 except idx=2 is 1 2") {
    assert(List(1, 2) == except(List(1, 2, 3), 2))
  }

  test("1 2 3 except idx=5 is 1 2 3") {
    assert(List(1, 2, 3) == except(List(1, 2, 3), 5))
  }

  def permutations[A](list: List[A]): List[List[A]] = {
    if (list.size < 2) List(list)
    else {
      {
        for {
          i <- (0 until list.size).toList
          xs <- permutations(except(list, i))
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

  test("permutations of 'hi' 'there' 'jack'") {
    val result = permutations(List("hi", "there", "jack"))
    assert(6 == result.size)
    assert(result.contains(List("hi", "there", "jack")))
    assert(result.contains(List("there", "jack", "hi")))
  }
}
