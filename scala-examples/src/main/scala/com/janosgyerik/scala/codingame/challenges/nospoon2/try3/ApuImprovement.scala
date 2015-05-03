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

    import GameState._

    val (nodes, neighbors, needs) = parseNodes(lines)
    getOptimalConnections(nodes, neighbors, needs).foreach(println)
  }
}

case class Node(rowNum: Int, colNum: Int)

case class Conn(n1: Node, n2: Node, num: Int) {
  override def toString: String = {
    "%s %s %s %s %s".format(n1.colNum, n1.rowNum, n2.colNum, n2.rowNum, num)
  }
}

object GameState {

  val emptyMarker = '.'

  def parseNodes(lines: Array[String]): (List[Node], Map[Node, List[Node]], Map[Node, Int]) = {
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
    val neighbors = nodeInfoList.groupBy(_._1).map {
      case (node, (_, _, right, down)) => node -> List(right, down).filter(_ != End)
    }
    val needs = nodeInfoList.map { case (node, need, _) => node -> need }.toMap
    (nodes, neighbors, needs)
  }

  def getOptimalConnections(nodes: List[Node], neighbors: Map[Node, List[Node]], needs: Map[Node, Int]): List[Conn] = {
    getConnections(nodes, neighbors, needs)
  }

  def getNormalizedConnections(connections: List[Conn]) = {
    connections.groupBy(_.n2).map {
      case (n2, conns2) => n2 -> conns2.map(_.num).sum
    }.map { case (n2, num) => Conn(connections.head.n1, n2, num) }.toList
  }

  def getConnections(nodes: List[Node], neighbors: Map[Node, List[Node]], needs: Map[Node, Int]): List[Conn] =
    nodes match {
      case Nil => Nil
      case x :: xs =>
        val connections = getNormalizedConnections(getConnections(x, neighbors, needs))
        val updatedNeeds = getUpdatedNeeds(connections, needs)
        connections ++ getConnections(xs, neighbors, updatedNeeds)
    }

  def getUpdatedNeeds(conns: List[Conn], needs: Map[Node, Int]): Map[Node, Int] = conns match {
    case Nil => needs
    case x :: xs => getUpdatedNeeds(xs, needs.updated(x.n2, needs(x.n2) - x.num))
  }

  def getConnections(node: Node, neighbors: Map[Node, List[Node]], needs: Map[Node, Int]): List[Conn] = {
    if (needs(node) == 0) Nil
    else if (node.right.needs > 0) Conn(node, node.right) :: getConnections(takeFromRight(node))
    else if (node.down.needs > 0) Conn(node, node.down) :: getConnections(takeFromDown(node))
    else throw new IllegalStateException("no more available neighbors")
  }

  def takeFromRight(node: Node) =
    Node(node.rowNum, node.colNum, node.needs - 1, node.right.satisfyOne, node.down)

  def takeFromDown(node: Node) =
    Node(node.rowNum, node.colNum, node.needs - 1, node.right, node.down.satisfyOne)

  def getRest(nodes: List[Node], connections: List[Conn]): List[Node] =
    for {
      n1 <- nodes
      conn <- connections
      n2 = conn.n2
      if n1.rowNum == n2.rowNum && n1.colNum == n2.colNum
    } yield n1.satisfy(conn.num)
}
