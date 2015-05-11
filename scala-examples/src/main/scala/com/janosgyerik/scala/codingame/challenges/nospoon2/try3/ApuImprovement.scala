package com.janosgyerik.scala.codingame.challenges.nospoon2.try3

import java.util.Scanner

object Player extends App {
  val scanner = new Scanner(System.in)

  solve(scanner)

  def solve(scanner: Scanner): Unit = {
    val width = scanner.nextInt
    val height = scanner.nextInt
    scanner.nextLine

    val lines = {
      for {_ <- List.range(0, height)} yield scanner.nextLine
    }.toArray
    lines.foreach(System.err.println)

    import GameState._

    val (nodes, needs, rightNeighbors, downNeighbors) = parseNodes(lines)
    getOptimalConnections(nodes, new GameState(needs, rightNeighbors, downNeighbors)).foreach(println)
  }
}

case class Node(rowNum: Int, colNum: Int)

case class Conn(n1: Node, n2: Node, num: Int = 1) {
  override def toString: String = {
    "%s %s %s %s %s".format(n1.colNum, n1.rowNum, n2.colNum, n2.rowNum, num)
  }
}

object GameState {

  val emptyMarker = '.'
  val maxConn = 2

  def parseNodes(lines: Array[String]): (List[Node], Map[Node, Int], Map[Node, Node], Map[Node, Node]) = {
    val width = lines(0).length
    val height = lines.length
    val End = Node(-1, -1)

    def parseNodeWithNeed(rowNum: Int, colNum: Int) =
      (Node(rowNum, colNum), lines(rowNum)(colNum) - '0')

    def findRightNeighbor(node: Node) = {
      val rowNum = node.rowNum
      val row = lines(rowNum)

      def findRightNeighbor(colNum: Int): Node = {
        if (colNum >= width) End
        else if (row(colNum) == emptyMarker) findRightNeighbor(colNum + 1)
        else Node(rowNum, colNum)
      }
      findRightNeighbor(node.colNum + 1)
    }

    def findDownNeighbor(node: Node) = {
      val colNum = node.colNum

      def findDownNeighbor(rowNum: Int): Node = {
        if (rowNum >= height) End
        else if (lines(rowNum)(colNum) == emptyMarker) findDownNeighbor(rowNum + 1)
        else Node(rowNum, colNum)
      }
      findDownNeighbor(node.rowNum + 1)
    }

    def parseNode(rowNum: Int, colNum: Int): (Node, Int, Node, Node) = {
      val char = lines(rowNum)(colNum)
      if (char == emptyMarker) (End, 0, End, End)
      else {
        val (node, need) = parseNodeWithNeed(rowNum, colNum)
        val right = findRightNeighbor(node)
        val down = findDownNeighbor(node)
        (node, need, right, down)
      }
    }

    val nodeInfoList = for {
      rowNum <- List.range(0, lines.length)
      colNum <- List.range(0, lines(0).length)
      (node, need, right, down) = parseNode(rowNum, colNum)
      if node != End
    } yield (node, need, right, down)

    val nodes = nodeInfoList.map(_._1)
    val needs = nodeInfoList.map { case (node, need, _, _) => node -> need }.toMap + (End -> 0)
//    val neighbors = nodeInfoList.groupBy(_._1).map {
//      case (node, (_, _, right, down)) => node -> List(right, down).filter(_ != End)
//    }
    val rightNeighbors = nodeInfoList.map { case (node, _, right, _) => node -> right }.toMap
    val downNeighbors = nodeInfoList.map { case (node, _, _, down) => node -> down }.toMap

    (nodes, needs, rightNeighbors, downNeighbors)
  }

  def getOptimalConnections(nodes: List[Node], gameState: GameState): List[Conn] = {
    getConnections(nodes, gameState)
  }

  def getNormalizedConnections(connections: List[Conn]) = {
    connections.groupBy(_.n2).map {
      case (n2, conns2) => n2 -> conns2.map(_.num).sum
    }.map { case (n2, num) => Conn(connections.head.n1, n2, num) }.toList
  }

  def getConnections(nodes: List[Node], gameState: GameState): List[Conn] = nodes match {
      case Nil => Nil
      case x :: xs =>
        val connections = getNormalizedConnections(getConnections(x, maxConn, maxConn, gameState))
        val updatedNeeds = getUpdatedNeeds(connections, gameState.needs)
        val updatedGameState = gameState.withUpdatedNeeds(updatedNeeds)
        connections ++ getConnections(xs, updatedGameState)
    }

  def getUpdatedNeeds(conns: List[Conn], needs: Map[Node, Int]): Map[Node, Int] = conns match {
    case Nil => needs
    case x :: xs => getUpdatedNeeds(xs, needs.updated(x.n2, needs(x.n2) - x.num))
  }

  def getConnections(node: Node, remainingRight: Int, remainingDown: Int, gameState: GameState): List[Conn] = {
    if (gameState.needs(node) == 0) Nil
    else if (remainingRight > 0 && gameState.canUseRight(node)) {
      Conn(node, gameState.getRight(node)) :: getConnections(
        node, remainingRight - 1, remainingDown, gameState.takeFromRight(node))
    } 
    else if (remainingDown > 0 && gameState.canUseDown(node)) {
      Conn(node, gameState.getDown(node)) :: getConnections(
        node, remainingRight, remainingDown - 1, gameState.takeFromDown(node))
    }
    else {
      throw new IllegalStateException("no more available neighbors")
    }
  }
}

class GameState(val needs: Map[Node, Int], rightNeighbors: Map[Node, Node], downNeighbors: Map[Node, Node]) {

  def withUpdatedNeeds(updatedNeeds: Map[Node, Int]) =
    new GameState(updatedNeeds, rightNeighbors, downNeighbors)

  def canUseRight(node: Node) = needs(rightNeighbors(node)) > 0
  
  def getRight(node: Node) = rightNeighbors(node)
  
  def canUseDown(node: Node) = needs(downNeighbors(node)) > 0

  def getDown(node: Node) = downNeighbors(node)

  private def takeFrom(node: Node) = {
    val updatedNeeds = needs.updated(node, needs(node) - 1)
    new GameState(updatedNeeds, rightNeighbors, downNeighbors)
  }

  def takeFromRight(node: Node) = {
    val right = rightNeighbors(node)
    takeFrom(right).takeFrom(node)
  }

  def takeFromDown(node: Node) = {
    val down = downNeighbors(node)
    takeFrom(down).takeFrom(node)
  }

  override def toString = s"GameState($needs)"
}
