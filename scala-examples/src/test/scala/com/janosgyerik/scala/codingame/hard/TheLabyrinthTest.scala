package com.janosgyerik.scala.codingame.hard

import com.janosgyerik.scala.codingame.hard.TheLabyrinth._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TheLabyrinthTest extends FunSuite {
  test("maze.start: unreachable in empty maze") {
    val game = new TheLabyrinth(Array(""), Unreachable, 10)
    assert(Unreachable == game.start)
  }

  test("maze.start: 0,0") {
    val game = new TheLabyrinth(Array("" + startMarker), Unreachable, 10)
    assert(new Pos(0,0) == game.start)
  }

  test("maze.start: 1,2") {
    val game = new TheLabyrinth(Array("...", "..T", "..."), Unreachable, 10)
    assert(new Pos(1,2) == game.start)
  }

  test("countScanAfterAction: nothing when map is complete") {
    val game = new TheLabyrinth(Array("...", "..T", "..."), Unreachable, 10)
    for {
      action <- allActions
    } yield {
      assert(0 == game.countScanAfterAction(action))
    }
  }

  test("countScanAfterAction: 5 in all directions") {
    val game = new TheLabyrinth(Array(
      "???????",
      "?.....?",
      "?.....?",
      "?..T..?",
      "?.....?",
      "?.....?",
      "???????"), new Pos(3, 3), 10)
    allActions.foreach(action => assert(5 == game.countScanAfterAction(action)))
  }

  test("countScanAfterAction: differnt for all directions") {
    val game = new TheLabyrinth(Array(
      "????..?",
      "?......",
      "?.....?",
      "...T..?",
      "......?",
      "......?",
      "????..?"), new Pos(3, 3), 10)
    assert(3 == game.countScanAfterAction(Up))
    assert(4 == game.countScanAfterAction(Right))
    assert(3 == game.countScanAfterAction(Down))
    assert(2 == game.countScanAfterAction(Left))
  }

}
