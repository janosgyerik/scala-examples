package com.janosgyerik.scala.codingame.medium

object Cabling {

  def parseInput(lines: Iterable[String]) = {
    lines.tail.map {
      line => {
        val parts = line.split(" ").take(2).map(x => x.toInt)
        (parts(0), parts(1))
      }
    }.toList
  }

  def rangeOfX(tuples: List[(Int, Int)]) = {
    val x = tuples.map { pair => pair._1 }
    (x.min, x.max)
  }

  def sortedY(tuples: List[(Int, Int)]) = {
    val y = tuples.map { pair => pair._2 }
    y.sorted
  }

  def sumDistancesFrom(ints: List[Int], from: Int) = {
    ints.map { x => Math.abs(from.asInstanceOf[Long] - x.asInstanceOf[Long]) }.sum
  }

  def minLength(coords: List[(Int, Int)]) = {
    val y = sortedY(coords)
    val range = rangeOfX(coords)
    range._2 - range._1 + {
      for {i <- y} yield sumDistancesFrom(y, i)
    }.min
  }

}
