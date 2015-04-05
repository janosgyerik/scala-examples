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

  def getSortedY(tuples: List[(Int, Int)]) = {
    tuples.map { pair => pair._2 }.sorted
  }

  def sumDistancesFrom(ints: List[Int], from: Int) = {
    val longval = from.asInstanceOf[Long]
    ints.map { x => Math.abs(longval - x.asInstanceOf[Long]) }.sum
  }

  def getMinXLength(coords: List[(Int, Int)]) = {
    val range = rangeOfX(coords)
    range._2 - range._1
  }

  def getMinYLength(coords: List[(Int, Int)]) = {
    val sortedY = getSortedY(coords)
    sortedY.map(y => sumDistancesFrom(sortedY, y)).min
  }

  def minLength(coords: List[(Int, Int)]) = {
    val xLength = getMinXLength(coords)
    val yLength = getMinYLength(coords)
    xLength + yLength
  }

}
