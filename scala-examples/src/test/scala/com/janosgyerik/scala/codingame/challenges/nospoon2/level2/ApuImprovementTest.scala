package com.janosgyerik.scala.codingame.challenges.nospoon2.level2

import java.util.Scanner

import com.janosgyerik.scala.codingame.challenges.nospoon2.level1.{Player, GameState}
import com.janosgyerik.scala.codingame.challenges.nospoon2.level1.GameState._
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

  test("normalized Conn") {
    assert(Conn(Node(1, 2), Node(2, 3)) == Conn.create(Node(1, 2), Node(2, 3)))
    assert(Conn(Node(1, 2), Node(2, 3)) == Conn.create(Node(2, 3), Node(1, 2)))
  }

  test("run example 1") {
    Player.solve(new Scanner("3\n3\n1.2\n...\n..1\n"))
  }

  test("run example 2") {
    Player.solve(new Scanner("2\n2\n2.\n42\n"))
  }

  test("takeFromRight") {
    val lines = Array("2.", "42")
    val nodes = parseNodes(lines)
    assert(Node(0,0,2,Node(-1,-1,0,null,null),Node(1,0,4,Node(-1,-1,0,null,null),Node(-1,-1,0,null,null)))
      == nodes.head)
    assert(Node(0,0,1,Node(-1,-1,0,null,null),Node(1,0,3,Node(-1,-1,0,null,null),Node(-1,-1,0,null,null)))
      == takeFromDown(nodes.head))
    assert(Node(0,0,0,Node(-1,-1,0,null,null),Node(1,0,2,Node(-1,-1,0,null,null),Node(-1,-1,0,null,null)))
      == takeFromDown(takeFromDown(nodes.head)))
  }

  test("getNormalizedConnections") {
    val conn1 = List(Conn(Node(1, 1), Node(2, 2), 1))
    assert(conn1 == getNormalizedConnections(conn1))
    assert(List(Conn(Node(1, 1), Node(2, 2), 3)) == getNormalizedConnections(conn1 ++ conn1 ++ conn1))
  }

}
