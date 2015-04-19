package com.janosgyerik.scala.codingame.hard

import com.janosgyerik.scala.codingame.hard.TheLabyrinth._

import scala.annotation.tailrec


object TheLabyrinth {

  abstract class Action

  case object Up extends Action

  case object Down extends Action

  case object Left extends Action

  case object Right extends Action

  type Maze = Array[String]

  abstract class Pos

  case class ReachablePos(row: Int, col: Int) extends Pos

  case object Unreachable extends Pos

  val startMarker = 'T'
  val targetMarker = 'C'

}

class TheLabyrinth(initialMaze: Maze, initialPos: Pos) {

  var maze = initialMaze
  var pos = initialPos
  var target = findPos(targetMarker)
  val start = findPos(startMarker)

  def updateMaze(maze: Maze): Unit = {
    this.maze = maze
    if (target == Unreachable) {
      target = findPos(targetMarker)
    }
  }

  def updatePos(pos: Pos): Unit = {
    this.pos = pos
  }

  @tailrec
  private def findPos(c: Char, row: Int): Pos = {
    if (row >= maze.size) Unreachable
    else
      maze(row) indexOf c match {
        case -1 => findPos(c, row + 1)
        case col => ReachablePos(row, col)
      }
  }

  def findPos(c: Char): Pos = findPos(c, 0)

  def isTargetVisible = target != Unreachable

  def isTargetReachable =
    findShortestPath(start, target).nonEmpty

  def findShortestPath(from: Pos, to: Pos): List[Action] = {
    to match {
      case Unreachable => List()
    }
  }
}
