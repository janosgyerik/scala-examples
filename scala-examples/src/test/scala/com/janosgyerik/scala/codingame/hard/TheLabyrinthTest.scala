package com.janosgyerik.scala.codingame.hard

import com.janosgyerik.scala.codingame.hard.TheLabyrinth._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TheLabyrinthTest extends FunSuite {
  test("maze.start: unreachable in empty maze") {
    val game = new TheLabyrinth(Array(""), Unreachable)
    assert(Unreachable == game.start)
  }

  test("maze.start: 0,0") {
    val game = new TheLabyrinth(Array("" + startMarker), Unreachable)
    assert(ReachablePos(0,0) == game.start)
  }

  test("maze.start: 1,2") {
    val game = new TheLabyrinth(Array("...", "..T", "..."), Unreachable)
    assert(ReachablePos(1,2) == game.start)
  }

}
