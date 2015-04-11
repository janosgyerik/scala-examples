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
    assert("Set(Link(0, 1), Link(1, 2), Link(2, 3), Link(2, 4))" == parseInput(inputScanner(1)).toString())
  }

  val testdir = "src/test/resources/codingame/medium/teads"

  def testInput(index: Int) = "%s/Test_%s_input.txt".format(testdir, index)

  def testOutput(index: Int) = "%s/Test_%s_output.txt".format(testdir, index)

  def inputScanner(index: Int): Scanner = new Scanner(new File(testInput(index)))

  def verifyInput(index: Int) = {
    assert(Source.fromFile(testOutput(index)).mkString.trim
      == solve(inputScanner(index)).toString)
  }

  for (index <- 1 to 10) {
    test("input" + index) {
      verifyInput(index)
    }
  }

  test("verify single input 1") {
    verifyInput(1)
  }

  def linksFromInput(index: Int) = parseInput(inputScanner(index))

}
