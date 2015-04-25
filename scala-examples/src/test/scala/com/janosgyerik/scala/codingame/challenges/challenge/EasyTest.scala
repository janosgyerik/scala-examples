package com.janosgyerik.scala.codingame.challenges.challenge

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class EasyTest extends FunSuite {

  import Solution._

  test("parseInput 1.2 ... ..1") {
    val input = Array("1.2", "...", "..1")

    val links = parseLinks(input)
    assert("List(Link(Node(0,0,1),Node(2,0,2)), Link(Node(2,0,2),Node(0,0,1)), Link(Node(2,0,2),Node(2,2,1)), Link(Node(2,2,1),Node(2,0,2)))"
      == links.toString)

    assert(Set(Node(0, 0, 1), Node(2, 2, 1), Node(2, 0, 2)) == getNodes(links))
  }

  test("findNeighbors 1.2 ... ..1") {
    val input = Array("1.2", "...", "..1")

    val links = parseLinks(input)
    val nodes = getNodes(links)
    assert(Set(Node(2, 0, 2)) == nodes.head.findNeighbors(links))
  }

  test("findNodesThatNeedAllPossibleConnections 1.2 ... ..1") {
    val input = Array("1.2", "...", "..1")

    val links = parseLinks(input)
    val nodes = getNodes(links)

    val game = new GameState(links, nodes)
    assert(Set(Node(2, 0, 2)) == game.findNodesThatNeedAllPossibleConnections)
  }
}
