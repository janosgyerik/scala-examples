package com.janosgyerik.scala.codingame.challenges.skynet

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class EasyTest extends FunSuite {

  test("initial inputs") {
    val inputs = InitialInputs.fromString("4 4 1\n0 1\n0 2\n1 3\n2 3\n3\n")

    assert(List((0, 1), (0, 2), (1, 3), (2, 3)) == inputs.nodeIdPairs)
    assert(List(3) == inputs.gwIds)

    val game = new Game(inputs)
    assert(Set(Node(0), Node(1), Node(2), Node(3)) == game.allNodes)
    assert(Set(
      Link(0, 1), Link(1, 0),
      Link(0, 2), Link(2, 0),
      Link(1, 3), Link(3, 1),
      Link(2, 3), Link(3, 2)
    ) == game.allLinks)

    assert(Set(Node(3)) == game.gwNodes)

    assert(Set(Link(1, 3), Link(2, 3)) == game.nodeLinksToGw)

    assert("1 3" == game.next(RoundInputs.fromString("0\n"))._1)

    val nextGame = game.next(RoundInputs.fromString("0\n"))._2
    assert(Set(
      Link(0, 1), Link(1, 0),
      Link(0, 2), Link(2, 0),
      Link(2, 3), Link(3, 2)
    ) == nextGame.allLinks)
  }

  test("round inputs") {
    val inputs = RoundInputs.fromString("2\n")
    assert(2 == inputs.agentNodeId)
  }

}
