package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

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

    // O(N^2)
    //sortedY.map(y => sumDistancesFrom(sortedY, y)).min

    val diffs = countSmaller(sortedY).zip(countBigger(sortedY)).map(x => Math.abs(x._1 - x._2))
    val bestSplitIndex = (0 until diffs.size).zip(diffs).minBy({ case (index, diff) => diff })._1
    sumDistancesFrom(sortedY, sortedY(bestSplitIndex))
  }

  def minLength(coords: List[(Int, Int)]) = {
    val xLength = getMinXLength(coords)
    val yLength = getMinYLength(coords)
    xLength + yLength
  }

  def countSmaller(ints: List[Int]) = {
    var count = 0
    var total = 0
    var current = ints.head
    for {
      x <- ints
    } yield {
      if (x != current) {
        count = total
        current = x
      }
      total = total + 1
      count
    }
  }

  def countBigger(ints: List[Int]) = {
    countSmaller(ints.reverse).reverse
  }

  def solve(scanner: Scanner): Long = {
    val linesCount = scanner.nextInt()
    scanner.nextLine()

    val lines = for {_ <- 1 to linesCount} yield scanner.nextLine()
    minLength(parseInput(List("") ++ lines))
  }

}
