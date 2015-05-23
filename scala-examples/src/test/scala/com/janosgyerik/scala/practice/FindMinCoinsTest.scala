package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FindMinCoinsTest extends FunSuite {

  def findMinCoins(amount: Int, coins: Set[Int]): Int = {
    def findMinCoins(amount: Int, count: Int): Int = {
      if (amount == 0) count
      else if (amount < 0) Integer.MAX_VALUE
      else coins.map(coin => findMinCoins(amount - coin, count + 1)).min
    }

    val count = findMinCoins(amount, 0)
    if (count == Integer.MAX_VALUE) 0
    else count
  }

  test("find 10 with 1, 5, 7") {
    assert(2 == findMinCoins(10, Set(1, 5, 7)))
  }

  test("find 10 with 1, 5, 10") {
    assert(1 == findMinCoins(10, Set(1, 5, 10)))
  }

  test("find 7 with 3, 5") {
    assert(0 == findMinCoins(7, Set(3, 5)))
  }

  test("find 6 with 1, 3, 4") {
    assert(2 == findMinCoins(6, Set(1, 3, 4)))
  }

  def findMinCoinsList(amount: Int, coins: Set[Int]): List[Int] = {
    def findMinCoinsList(amount: Int, list: List[Int]): (Boolean, List[Int]) = {
      if (amount == 0) (true, list)
      else if (amount < 0) (false, Nil)
      else {
        val solutions = coins.map(coin => findMinCoinsList(amount - coin, coin :: list)).filter(_._1)
        if (solutions.nonEmpty) solutions.minBy(_._2.size)
        else (false, Nil)
      }
    }

    findMinCoinsList(amount, Nil)._2.sorted
  }

  test("find list 10 with 1, 5, 7") {
    assert(List(5, 5) == findMinCoinsList(10, Set(1, 5, 7)))
  }

  test("find list 10 with 1, 5, 10") {
    assert(List(10) == findMinCoinsList(10, Set(1, 5, 10)))
  }

  test("find list 7 with 3, 5") {
    assert(Nil == findMinCoinsList(7, Set(3, 5)))
  }

  test("find list 6 with 1, 3, 4") {
    assert(List(3, 3) == findMinCoinsList(6, Set(1, 3, 4)))
  }

  test("find list 7 with 1, 3, 4") {
    assert(List(3, 4) == findMinCoinsList(7, Set(1, 3, 4)))
  }

}
