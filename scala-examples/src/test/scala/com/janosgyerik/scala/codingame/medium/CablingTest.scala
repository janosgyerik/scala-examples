package com.janosgyerik.scala.codingame.medium

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ArrayBuffer

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

  test("parseInput 3, 0 0, 1 1, 2 2") {
    assert(List((0, 0), (1, 1), (2, 2)) == parseInput(Array("3", "0 0", "1 1", "2 2").toIterable))
  }

  test("parseInput 4, 1 2, 0 0, 2 2, 1 3") {
    assert(List((1, 2), (0, 0), (2, 2), (1, 3)) == parseInput(Array("4", "1 2", "0 0", "2 2", "1 3").toIterable))
  }
}
