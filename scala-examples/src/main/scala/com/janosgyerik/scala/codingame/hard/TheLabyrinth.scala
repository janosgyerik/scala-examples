package com.janosgyerik.scala.codingame.hard

import com.janosgyerik.scala.codingame.hard.TheLabyrinth._

import scala.annotation.tailrec


object TheLabyrinth {

  val scanRange = 2

  abstract class Action(val deltaRow: Int, val deltaCol: Int)

  case object Up extends Action(-1, 0)

  case object Down extends Action(1, 0)

  case object Left extends Action(0, -1)

  case object Right extends Action(0, 1)

  val allActions = Set(Up, Down, Left, Right)

  type Maze = Array[String]

  class Pos(val row: Int, val col: Int) {
    def +(action: Action) = {
      new Pos(row + action.deltaRow, col + action.deltaCol)
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Pos]

    override def equals(other: Any): Boolean = other match {
      case that: Pos =>
        (that canEqual this) &&
          row == that.row &&
          col == that.col
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(row, col)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  case object Unreachable extends Pos(-1, -1)

  type Marker = Char

  val wallMarker = '#'
  val startMarker = 'T'
  val targetMarker = 'C'
  val unknownMarker = '?'

}

class TheLabyrinth(initialMaze: Maze, timeToAlarm: Int) {

  var maze = initialMaze
  val start = findPos(startMarker)
  var pos = start
  var target = findPos(targetMarker)

  // used while exploring, to avoid cyclical logic
  // (keep going until waypoint reached before recalculating shortest paths)
  var waypoint = List[Action]()

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
        case col => new Pos(row, col)
      }
  }

  def findPos(c: Char): Pos = findPos(c, 0)

  def isTargetVisible = target != Unreachable

  def isTargetReachable =
    findShortestPath(start, target).nonEmpty

  def findShortestPath(from: Pos, to: Pos): List[Action] = ???

  def findShortestPath(from: Pos, marker: Marker): List[Action] = {
    type Branch = (Pos, List[Action])

    def findShortedPath(visited: Set[Pos], branches: List[Branch]): List[Action] = {
      val newBranches = for {
        branch <- branches
        action <- allActions if {
          pos = branch._1 + action
          !visited.contains(pos) && isValidPos(pos)
        }
      } yield (pos, action :: branch._2)

      if (newBranches.isEmpty) {
        return List.empty
      }

      var index = 0
      while (index < newBranches.size) {
        newBranches(index) match {
          case (x, path) => if (maze(x.row)(x.col) == marker) return path.reverse
        }
        index = index + 1
      }

      val newPos = newBranches.map { case (x, _) => x }.toSet
      findShortedPath(visited ++ newPos, newBranches)
    }
    findShortedPath(Set.empty, List((from, List.empty)))
  }

  def isValidPos(pos: Pos) = {
    def withinRange(x: Int, end: Int) = 0 <= x && x < end
    withinRange(pos.row, maze.size) &&
      withinRange(pos.col, maze(0).length) &&
      maze(pos.row)(pos.col) != wallMarker
  }

  def isValidAction(action: Action) = isValidPos(pos + action)

  def getValidActions = {
    allActions.filter(isValidAction)
  }

  def isUnknownPos(row: Int, col: Int) = maze(row)(col) == unknownMarker

  def countScanAfterAction(action: Action) = {
    val posAfterAction = pos + action

    val startRow = math.max(0, posAfterAction.row - scanRange)
    val endRow = math.min(maze.size - 1, posAfterAction.row + scanRange)
    val startCol = math.max(0, posAfterAction.col - scanRange)
    val endCol = math.min(maze(0).length - 1, posAfterAction.col + scanRange)

    {
      for {
        row <- startRow to endRow
        col <- startCol to endCol if isUnknownPos(row, col)
      } yield true
    }.size
  }

  def findActionToMaxScan = {
    val actionScanPairs = for {
      action <- getValidActions
    } yield (action, countScanAfterAction(action))

    actionScanPairs.maxBy(_._2)
  }

  def getNextMoveToTarget = findShortestPath(pos, target).head

  def isTooFarFromStart = findShortestPath(start, pos).size >= timeToAlarm - scanRange

  def updateWaypointAndExplore() = {
    val newWaypoint =
      if (waypoint.nonEmpty) waypoint
      else if (isTooFarFromStart) findShortestPath(start, unknownMarker)
      else findShortestPath(pos, unknownMarker)

    waypoint = newWaypoint.tail
    newWaypoint.head
  }

  def getNextMove = {
    // TODO:
    //if (hasAlreadyReachedTarget) getNextMoveToStart
    if (isTargetVisible && isTargetReachable) getNextMoveToTarget
    else updateWaypointAndExplore()
  }
}
