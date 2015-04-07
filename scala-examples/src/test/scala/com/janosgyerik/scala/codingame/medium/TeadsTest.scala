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
    assert("Vector((0,1), (1,2), (2,3), (2,4))" == parseInput(inputScanner(1)).toString())
  }

  val testdir = "src/test/resources/codingame/medium/teads"

  def testInput(index: Int) = "%s/Test_%s_input.txt".format(testdir, index)

  def testOutput(index: Int) = "%s/Test_%s_output.txt".format(testdir, index)

  def inputScanner(index: Int): Scanner = new Scanner(new File(testInput(index)))

  for (i <- 1 to 10) {
    test("input" + i) {
      assert(Source.fromFile(testOutput(i)).mkString
        == solve(inputScanner(i)).toString)
    }
  }

}
