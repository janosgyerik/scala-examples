package com.janosgyerik.scala.codingame.challenges.nospoon2.level1

import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ApuInitTest extends FunSuite {

  import GameState._

  test("example 1") {
    val lines = Array("1.2", "...", "..1")
    val nodes = parseNodes(lines)
    assert(List("0 0 2 0 -1 -1", "2 0 -1 -1 2 2", "2 2 -1 -1 -1 -1") == toFormattedLines(nodes))
  }

  test("run example 1") {
    Player.solve(new Scanner("3\n3\n1.2\n...\n..1\n"))
  }
}
