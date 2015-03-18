package com.janosgyerik.scala.practice

object SquareSum {

  def squaresUpTo(n: Int) = 1.to(n).filter(x => x * x <= n).map(x => x * x)

  def minSquaresSummingTo(n: Int): List[Int] = {
    val squares = squaresUpTo(n)

    def inner(n: Int): List[Int] = {
      if (n < 1) List()
      else if (n == 1) List(1)
      else squares.filter(x => x <= n).map(x => List(x) ++ inner(n - x)).minBy(list => list.size).sorted
    }
    inner(n)
  }

}
