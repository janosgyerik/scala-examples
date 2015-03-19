package com.janosgyerik.scala.practice

object SquareSum {

  def squaresUpTo(n: Int): List[Int] = (1 to n).filter(x => x * x <= n).map(x => x * x).toList

  def minSquaresSummingTo(n: Int): Int = {
    def inner(minTermCount: List[Int], squares: List[Int]): Int = squares match {
      case List() => minTermCount.last
      case _ =>
        val sq = squares.head
        val newMinTermCount = (0 to n).map(x => Math.min(minTermCount(x), x / sq + minTermCount(x % sq))).toList
        inner(newMinTermCount, squares drop 1)
    }
    val minTermCount = (0 to n).toList
    val squares = squaresUpTo(n) drop 1
    inner(minTermCount, squares)
  }

}
