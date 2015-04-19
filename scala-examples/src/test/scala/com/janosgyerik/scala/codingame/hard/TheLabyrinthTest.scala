package com.janosgyerik.scala.codingame.hard

import com.janosgyerik.scala.codingame.hard.TheLabyrinth._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TheLabyrinthTest extends FunSuite {
  test("maze.start: unreachable in empty maze") {
    val game = new TheLabyrinth(Array(""), 10)
    assert(Unreachable == game.start)
  }

  test("maze.start: 0,0") {
    val game = new TheLabyrinth(Array("" + startMarker), 10)
    assert(new Pos(0,0) == game.start)
  }

  test("maze.start: 1,2") {
    val game = new TheLabyrinth(Array("...", "..T", "..."), 10)
    assert(new Pos(1,2) == game.start)
  }

  test("countScanAfterAction: nothing when map is complete") {
    val game = new TheLabyrinth(Array("...", "..T", "..."), 10)
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
      "???????"), 10)
    allActions.foreach(action => assert(5 == game.countScanAfterAction(action)))
  }

  test("countScanAfterAction: different for all directions") {
    val game = new TheLabyrinth(Array(
      "????..?",
      "?......",
      "?.....?",
      "...T..?",
      "......?",
      "......?",
      "????..?"), 10)
    assert(3 == game.countScanAfterAction(Up))
    assert(4 == game.countScanAfterAction(Right))
    assert(3 == game.countScanAfterAction(Down))
    assert(2 == game.countScanAfterAction(Left))

    assert(Right == game.findActionToMaxScan._1)
  }

  test("getValidActions: all") {
    val game = new TheLabyrinth(Array(
      "...",
      ".T.",
      "..."), 10)
    assert(allActions == game.getValidActions)
  }

  test("getValidActions: top left corner") {
    val game = new TheLabyrinth(Array(
      "T..",
      "...",
      "..."), 10)
    assert(Set(Right, Down) == game.getValidActions)
  }

  test("getValidActions: bottom edge") {
    val game = new TheLabyrinth(Array(
      "...",
      "...",
      ".T."), 10)
    assert(Set(Up, Right, Left) == game.getValidActions)
  }

  test("getValidActions: wall to left and up") {
    val game = new TheLabyrinth(Array(
      ".#.",
      "#T.",
      "..."), 10)
    assert(Set(Right, Down) == game.getValidActions)
  }

  test("getValidActions: none") {
    val game = new TheLabyrinth(Array(
      ".#.",
      "#T#"), 10)
    assert(Set() == game.getValidActions)
  }

  test("findShortestPath: straight to the right") {
    val game = new TheLabyrinth(Array(
      "......",
      ".T..C.",
      "......"
    ), 10)
    assert(game.isTargetVisible)
    assert(List(Right, Right, Right) == game.findShortestPath(game.pos, 'C'))
  }

  test("findShortestPath: down and right") {
    val game = new TheLabyrinth(Array(
      "......",
      ".T....",
      "....C."
    ), 10)
    assert(game.isTargetVisible)
    assert(List(Down, Right, Right, Right) == game.findShortestPath(game.pos, 'C'))
  }

  test("findShortestPath: to the right through obstacle") {
    val game = new TheLabyrinth(Array(
      "......",
      ".T.#C.",
      "......"
    ), 10)
    assert(game.isTargetVisible)
    assert(List(Up, Right, Right, Right, Down) == game.findShortestPath(game.pos, 'C'))
  }

  test("findShortestPath: down and around") {
    val game = new TheLabyrinth(Array(
      "....#.",
      ".T.#C.",
      "...##.",
      "......"
    ), 10)
    assert(game.isTargetVisible)
    assert(List(Down, Down, Right, Right, Right, Right, Up, Up, Left) == game.findShortestPath(game.pos, 'C'))
  }

}
