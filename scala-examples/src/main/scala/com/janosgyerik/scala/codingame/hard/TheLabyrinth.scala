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

  class Maze(rows: Array[String]) {
    require(rows.length > 0)

    import Maze._

    val height = rows.length
    val width = rows(0).length

    val start = findPos(startMarker)
    val target = findPos(targetMarker)

    @tailrec
    private def findPos(c: Char, row: Int): Pos = {
      if (row >= height) Unreachable
      else
        rows(row) indexOf c match {
          case -1 => findPos(c, row + 1)
          case col => new Pos(row, col)
        }
    }

    def findPos(c: Char): Pos = findPos(c, 0)

    def apply(row: Int) = rows(row)

    def apply(row: Int, col: Int) = rows(row)(col)

    def apply(pos: Pos) = rows(pos.row)(pos.col)

    def isValidPos(pos: Pos) = {
      def withinRange(x: Int, end: Int) = 0 <= x && x < end
      withinRange(pos.row, height) &&
        withinRange(pos.col, width) &&
        rows(pos.row)(pos.col) != wallMarker
    }

    def isUnknownPos(row: Int, col: Int) = this(row, col) == unknownMarker
  }

  object Maze {
    val wallMarker = '#'
    val startMarker = 'T'
    val targetMarker = 'C'
    val unknownMarker = '?'

    def fromLines(rows: String*) = new Maze(rows.toArray)
  }

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

}

class TheLabyrinth(initialMaze: Maze, alarm: Int = 0) {

  var maze = initialMaze
  val start = maze.start
  var pos = start
  var target = maze.target

  val timeToAlarm = if (alarm > 0) alarm else Integer.MAX_VALUE

  // used while exploring, to avoid cyclical logic
  // (keep going until waypoint reached before recalculating shortest paths)
  var waypoint = List[Action]()

  def updateMaze(maze: Maze): Unit = {
    this.maze = maze
    if (target == Unreachable) {
      target = maze.target
    }
  }

  def updatePos(pos: Pos): Unit = {
    this.pos = pos
  }

  def isTargetVisible = target != Unreachable

  def isTargetReachable =
    findShortestPath(start, target).nonEmpty

  def findShortestPath(from: Pos, to: Pos): List[Action] = ???

  def findShortestPath(from: Pos, marker: Marker): List[Action] = {
    type Branch = (Pos, List[Action])

    @tailrec
    def findShortestPath(visited: Set[Pos], branches: List[Branch]): List[Action] = {
      val newBranches = for {
        branch <- branches
        action <- allActions
        pos = branch._1 + action
        if !visited.contains(pos) && maze.isValidPos(pos)
      } yield (pos, action :: branch._2)

      if (newBranches.isEmpty) {
        return List.empty
      }

      val matchingPaths = for {
        (pos, path) <- newBranches
        if maze(pos) == marker
      } yield path.reverse

      if (matchingPaths.nonEmpty) matchingPaths.head
      else {
        val newPos = newBranches.map { case (x, _) => x }.toSet
        findShortestPath(visited ++ newPos, newBranches)
      }
    }
    findShortestPath(Set.empty, List((from, List.empty)))
  }

  def isValidAction(action: Action) = maze.isValidPos(pos + action)

  def getValidActions = {
    allActions.filter(isValidAction)
  }

  def countScanAfterAction(action: Action) = {
    val posAfterAction = pos + action

    val startRow = math.max(0, posAfterAction.row - scanRange)
    val endRow = math.min(maze.height - 1, posAfterAction.row + scanRange)
    val startCol = math.max(0, posAfterAction.col - scanRange)
    val endCol = math.min(maze.width - 1, posAfterAction.col + scanRange)

    {
      for {
        row <- startRow to endRow
        col <- startCol to endCol if maze.isUnknownPos(row, col)
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
      else if (isTooFarFromStart) findShortestPath(start, Maze.unknownMarker)
      else findShortestPath(pos, Maze.unknownMarker)

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
