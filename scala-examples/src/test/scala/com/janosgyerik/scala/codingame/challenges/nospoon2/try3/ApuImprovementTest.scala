package com.janosgyerik.scala.codingame.challenges.nospoon2.try3

import java.util.Scanner

import com.janosgyerik.scala.codingame.challenges.nospoon2.level1.{GameState, Player}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ApuImprovementTest extends FunSuite {

  import GameState._

  val lines = Array("1.2", "...", "..1")
  val nodes = parseNodes(lines)

  test("example 1") {
//    assert(isValidSolution(nodes, getOptimalConnections(nodes)))
  }

  test("run example 1") {
    Player.solve(new Scanner("3\n3\n1.2\n...\n..1\n"))
  }

  test("run example 2") {
    Player.solve(new Scanner("2\n2\n2.\n42\n"))
  }

  test("getNormalizedConnections") {
    val conn1 = List(Conn(Node(1, 1), Node(2, 2), 1))
    assert(conn1 == getNormalizedConnections(conn1))
    assert(List(Conn(Node(1, 1), Node(2, 2), 3)) == getNormalizedConnections(conn1 ++ conn1 ++ conn1))
  }

}
