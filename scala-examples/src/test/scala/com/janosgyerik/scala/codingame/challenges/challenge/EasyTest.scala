package com.janosgyerik.scala.codingame.challenges.challenge

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import java.util.Scanner

@RunWith(classOf[JUnitRunner])
class EasyTest extends FunSuite {

  import Solution._

  val input = Array("1.2", "...", "..1")
  val links = parseLinks(input)
  val nodes = getNodes(links)

  test("parseInput example1") {
    assert("List(Link(Node(0,0,1),Node(2,0,2)), Link(Node(2,0,2),Node(0,0,1)), Link(Node(2,0,2),Node(2,2,1)), Link(Node(2,2,1),Node(2,0,2)))"
      == links.toString)

    assert(Set(Node(0, 0, 1), Node(2, 2, 1), Node(2, 0, 2)) == getNodes(links))
  }

  test("findNeighbors example1") {
    assert(Set(Node(2, 0, 2)) == nodes.head.findNeighbors(links))
    assert(Set(Node(0, 0, 1), Node(2, 2, 1)) == Node(2, 0, 2).findNeighbors(links))
  }

  test("needAllPossibleConnections exmaple1") {
    assert(Node(2, 0, 2).needsAllPossibleConnections(links))
  }

  test("findNodesThatNeedAllPossibleConnections example1") {
    val game = new GameState(links, nodes)
    assert(Set(Node(2, 0, 2)) == game.findNodesThatNeedAllPossibleConnections)
  }

  test("getAllPossibleConnections example1") {
    val game = new GameState(links, nodes)
    assert(Set((Link(Node(2,0,2),Node(0,0,1)),1), (Link(Node(2,0,2),Node(2,2,1)),1))
      == game.getAllPossibleConnections(Node(2, 0, 2)))
  }

  test("getProvidableCount") {
    assert(1 == Node(0, 0, 1).getProvidableCount)
    assert(2 == Node(2, 0, 2).getProvidableCount)
    assert(2 == Node(2, 0, 3).getProvidableCount)
    assert(0 == Node(2, 0, 0).getProvidableCount)
  }

  test("computeNeedReductions") {
    val game = new GameState(links, nodes)
    val node = Node(2, 0, 2)
    val connections = game.getAllPossibleConnections(node)
    assert(Map(Node(2,2,1) -> 1, Node(2,0,2) -> 2, Node(0,0,1) -> 1) == game.computeNeedReductions(connections))
  }

  test("computeNodeReductionMap") {
    val game = new GameState(links, nodes)
    val node = Node(2, 0, 2)
    val connections = game.getAllPossibleConnections(node)
    val reductions = game.computeNeedReductions(connections)
    assert(Map(Node(2,2,1) -> Node(2,2,0), Node(2,0,2) -> Node(2,0,0), Node(0,0,1) -> Node(0,0,0)) ==
      game.computeNodeReductionMap(reductions))
  }

  test("removeConnections") {
    val game = new GameState(links, nodes)
    val node = Node(2, 0, 2)
    val connections = game.getAllPossibleConnections(node)
    val game2 = game.removeConnections(connections)
    Player.solve(new Scanner("3\n3\n1.2\n...\n..1\n"))
    assert(game2.nodes.isEmpty)
  }

  test("solve example2") {
    val game = new GameState(links, nodes)
    // starts well, but doesn't terminate
//    Player.solve(new Scanner("2\n2\n42\n31"))
  }
}
