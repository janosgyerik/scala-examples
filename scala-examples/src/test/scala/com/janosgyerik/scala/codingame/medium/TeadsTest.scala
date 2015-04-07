package com.janosgyerik.scala.codingame.medium

import java.io.File
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class TeadsTest extends FunSuite {

  import com.janosgyerik.scala.codingame.medium.Teads._

  test("parseInput, input1") {
    assert("Vector(Link(0, 1, 1), Link(1, 2, 1), Link(2, 3, 1), Link(2, 4, 1), " +
      "Link(1, 0, 1), Link(2, 1, 1), Link(3, 2, 1), Link(4, 2, 1))" == parseInput(inputScanner(1)).toString())
  }

  val testdir = "src/test/resources/codingame/medium/teads"

  def testInput(index: Int) = "%s/Test_%s_input.txt".format(testdir, index)

  def testOutput(index: Int) = "%s/Test_%s_output.txt".format(testdir, index)

  def inputScanner(index: Int): Scanner = new Scanner(new File(testInput(index)))

  def verifyInput(index: Int) = {
    assert(Source.fromFile(testOutput(index)).mkString
      == solve(inputScanner(index)).toString)
  }

//  for (index <- 1 to 10) {
  for (index <- 1 to 7) {
    test("input" + index) {
      verifyInput(index)
    }
  }

}
