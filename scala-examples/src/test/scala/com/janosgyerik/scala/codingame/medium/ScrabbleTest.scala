package com.janosgyerik.scala.codingame.medium

import java.io.File
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source


@RunWith(classOf[JUnitRunner])
class ScrabbleTest extends FunSuite {

  import com.janosgyerik.scala.codingame.medium.Scrabble._

  val testdir = "src/test/resources/codingame/medium/scrabble"

  for (i <- 1 to 1) {
    test("input" + i) {
      assert(Source.fromFile(testdir + "/out" + i + ".txt").mkString.trim
        == solve(new Scanner(new File(testdir + "/in" + i + ".txt"))).toString)
    }
  }

}
