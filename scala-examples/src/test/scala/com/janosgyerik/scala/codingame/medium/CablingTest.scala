package com.janosgyerik.scala.codingame.medium

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CablingTest extends FunSuite {
  //  import com.janosgyerik.scala.coursera1.Sqrt.sqrt

  def parseInput(lines: Iterable[String]) = {
    lines.tail.map {
      line => {
        val parts = line.split(" ").take(2).map(x => x.toInt)
        (parts(0), parts(1))
      }
    }.toList
  }

  val input1 = Array("3", "0 0", "1 1", "2 2")
  val input3 = Array("4", "1 2", "0 0", "2 2", "1 3")
  val input4 = Array("1", "1 1")
  val input5 = Array("3", "-5 -3", "-9 2", "3 -4")
  val input6 = Array("8",
    "-28189131 593661218",
    "102460950 1038903636",
    "938059973 -816049599",
    "-334087877 -290840615",
    "842560881 -116496866",
    "-416604701 690825290",
    "19715507 470868309",
    "846505116 -694479954"
  )

  test("parseInput 3, 0 0, 1 1, 2 2") {
    assert(List((0, 0), (1, 1), (2, 2)) == parseInput(input1.toIterable))
  }

  test("parseInput 4, 1 2, 0 0, 2 2, 1 3") {
    assert(List((1, 2), (0, 0), (2, 2), (1, 3)) == parseInput(input3.toIterable))
  }

  def rangeOfX(tuples: List[(Int, Int)]) = {
    val x = tuples.map { pair => pair._1 }
    (x.min, x.max)
  }

  test("rangeOfX, input1") {
    val coords = parseInput(input1)
    assert((0, 2) == rangeOfX(coords))
  }

  test("rangeOfX, input3") {
    val coords = parseInput(input3)
    assert((0, 2) == rangeOfX(coords))
  }

  def sortedY(tuples: List[(Int, Int)]) = {
    val y = tuples.map { pair => pair._2 }
    y.sorted
  }

  test("sortedY, input1") {
    assert(List(0, 1, 2) == sortedY(parseInput(input1)))
  }

  test("sortedY, input3") {
    assert(List(0, 2, 2, 3) == sortedY(parseInput(input3)))
  }

  def sumDistancesFrom(ints: List[Int], from: Int) = {
    ints.map { x => Math.abs(from - x) }.sum
  }

  test("sumDistancesFrom, input1") {
    val y = sortedY(parseInput(input1))
//    for { i <- y } println(sumDistancesFrom(y, i))
  }

  test("sumDistancesFrom, input3") {
    val y = sortedY(parseInput(input3))
//    for { i <- y } println(sumDistancesFrom(y, i))
  }

  def minLength(coords: List[(Int, Int)]) = {
    val y = sortedY(coords)
    val range = rangeOfX(coords)
    range._2 - range._1 + { for { i <- y } yield sumDistancesFrom(y, i) }.min
  }

  test("minLength, input1") {
    assert(4 == minLength(parseInput(input1)))
  }

  test("minLength, input3") {
    assert(5 == minLength(parseInput(input3)))
  }

  test("minLength, input4") {
    assert(0 == minLength(parseInput(input4)))
  }

  test("minLength, input5") {
    assert(18 == minLength(parseInput(input5)))
  }

  test("minLength, input6") {
    assert(6066790161L == minLength(parseInput(input6)))
  }
}
