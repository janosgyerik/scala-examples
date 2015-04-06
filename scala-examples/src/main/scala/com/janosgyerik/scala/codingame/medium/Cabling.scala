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
    // O(N^2)
    //sortedY.map(y => sumDistancesFrom(sortedY, y)).min
    // 0 0 0 1 2 3 3 3 3 4 4 5 5 5 5 7 8 9
    def countCurrent(startIndex: Int, current: Int) = sortedY.drop(startIndex).takeWhile(x => x == current).size

    def findCloseToEvenPartitionIndex(startIndex: Int, leftCount: Int, currentCount: Int, rightCount: Int): Int = {
      if (leftCount >= rightCount) startIndex
      else {
        val current = sortedY(startIndex)
        val nextCurrentCount = countCurrent(startIndex, current)
        findCloseToEvenPartitionIndex(0, leftCount + currentCount, nextCurrentCount, rightCount - nextCurrentCount)
      }
    }
    val currentCount = countCurrent(0, sortedY.head)
    val index = findCloseToEvenPartitionIndex(currentCount - 1, 0, currentCount, sortedY.size - currentCount)
    if (index > 0) {
      if (index < sortedY.size - 1) {
        (index - 1 to index + 1).map(y => sumDistancesFrom(sortedY, sortedY(y))).min
      } else {
        (index - 1 to index).map(y => sumDistancesFrom(sortedY, sortedY(y))).min
      }
    } else {
      if (index < sortedY.size - 1) {
        (index to index + 1).map(y => sumDistancesFrom(sortedY, sortedY(y))).min
      } else {
        (index to index).map(y => sumDistancesFrom(sortedY, sortedY(y))).min
      }
    }
  }

  def minLength(coords: List[(Int, Int)]) = {
    val xLength = getMinXLength(coords)
    val yLength = getMinYLength(coords)
    xLength + yLength
  }

  def countSmaller(ints: List[Int]) = {
    def inner(remaining: List[Int], current: Int, count: Int, total: Int): List[Int] = remaining match {
      case List() => List()
      case x :: xs =>
        if (x == current) List(count) ++ inner(xs, current, count, total + 1)
        else List(total) ++ inner(xs, x, total, total + 1)
    }
    List(0) ++ inner(ints.tail, ints.head, 0, 1)
  }

}
