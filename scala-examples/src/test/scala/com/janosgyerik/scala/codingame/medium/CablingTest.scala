package com.janosgyerik.scala.codingame.medium

import java.io.File
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source


@RunWith(classOf[JUnitRunner])
class CablingTest extends FunSuite {

  import com.janosgyerik.scala.codingame.medium.Cabling._

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

  test("rangeOfX, input1") {
    val coords = parseInput(input1)
    assert((0, 2) == rangeOfX(coords))
  }

  test("rangeOfX, input3") {
    val coords = parseInput(input3)
    assert((0, 2) == rangeOfX(coords))
  }

  test("sortedY, input1") {
    assert(List(0, 1, 2) == getSortedY(parseInput(input1)))
  }

  test("sortedY, input3") {
    assert(List(0, 2, 2, 3) == getSortedY(parseInput(input3)))
  }

  test("sumDistancesFrom, input1") {
    val y = getSortedY(parseInput(input1))
    assert(List(3, 2, 3) == {
      for {i <- y} yield sumDistancesFrom(y, i)
    })
  }

  test("sumDistancesFrom, input3") {
    val y = getSortedY(parseInput(input3))
    assert(List(7, 3, 3, 5) == {
      for {i <- y} yield sumDistancesFrom(y, i)
    })
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

  test("countSmaller 0 0 1 1 1 2 3 4 4 4 4 4 5") {
    assert(List(0, 0, 2, 2, 2, 5, 6, 7, 7, 7, 7, 7, 12) == countSmaller(List(0, 0, 1, 1, 1, 2, 3, 4, 4, 4, 4, 4, 5)))
  }

  test("countBigger 0 0 1 1 1 2 3 4 4 4 4 4 5") {
    assert(List(11, 11, 8, 8, 8, 7, 6, 1, 1, 1, 1, 1, 0) == countBigger(List(0, 0, 1, 1, 1, 2, 3, 4, 4, 4, 4, 4, 5)))
  }

  val testdir = "src/test/resources/codingame/medium/cabling"

  for (i <- 1 to 9) {
    test("input" + i) {
      assert(Source.fromFile(testdir + "/out" + i + ".txt").mkString.trim
        == solve(new Scanner(new File(testdir + "/in" + i + ".txt"))).toString)
    }
  }

}
