package com.janosgyerik.scala.codingame.challenges.nospoon2.level2

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

    getOptimalConnections(parseNodes(lines)).foreach(println)
  }
}

object Node {
  val End = Node(-1, -1, 0, null, null)
}

case class Node(rowNum: Int, colNum: Int, needs: Int = 0, right: Node = Node.End, down: Node = Node.End) {

  def <(other: Node) = {
    rowNum < other.rowNum || rowNum == other.rowNum && colNum < other.colNum
  }

  def satisfyWithRight =
    Node(rowNum, colNum, needs - 1, right.satisfyOne, down)

  def satisfyWithDown =
    Node(rowNum, colNum, needs - 1, right, down.satisfyOne)

  def satisfyWithBoth =
    Node(rowNum, colNum, needs - 2, right.satisfyOne, down.satisfyOne)

  def satisfy(n: Int): Node =
    Node(rowNum, colNum, needs - n, right, down)

  def satisfyOne: Node =
    satisfy(1)

  override def toString =
    s"Node($rowNum, $colNum, $needs)"
}

case class Link(n1: Node, n2: Node)

object Conn {
  def create(node1: Node, node2: Node) = {
    val (n1, n2) = if (node1 < node2) (node1, node2) else (node2, node1)
    Conn(n1, n2)
  }
}

case class Conn(n1: Node, n2: Node, num: Int = 1) {
  override def toString: String = {
    "%s %s %s %s %s".format(n1.colNum, n1.rowNum, n2.colNum, n2.rowNum, num)
  }
}

object GameState {

  val emptyMarker = '.'

  def parseNodes(lines: Array[String]): List[Node] = {
    val width = lines(0).length
    val height = lines.length

    def findRightNeighbor(node: Node) = {
      val rowNum = node.rowNum
      val row = lines(rowNum)

      def findRightNeighbor(colNum: Int): Node = {
        if (colNum >= width) Node.End
        else if (row(colNum) == emptyMarker) findRightNeighbor(colNum + 1)
        else Node(rowNum, colNum, lines(rowNum)(colNum) - '0')
      }
      findRightNeighbor(node.colNum + 1)
    }

    def findDownNeighbor(node: Node) = {
      val colNum = node.colNum

      def findDownNeighbor(rowNum: Int): Node = {
        if (rowNum >= height) Node.End
        else if (lines(rowNum)(colNum) == emptyMarker) findDownNeighbor(rowNum + 1)
        else Node(rowNum, colNum, lines(rowNum)(colNum) - '0')
      }
      findDownNeighbor(node.rowNum + 1)
    }

    def parseNode(rowNum: Int, colNum: Int): Node = {
      val char = lines(rowNum)(colNum)
      if (char == emptyMarker) Node.End
      else {
        val node = Node(rowNum, colNum, lines(rowNum)(colNum))
        Node(rowNum, colNum, lines(rowNum)(colNum) - '0', findRightNeighbor(node), findDownNeighbor(node))
      }
    }
    {
      for {
        rowNum <- List.range(0, lines.length)
        colNum <- List.range(0, lines(0).length)
      } yield parseNode(rowNum, colNum)
    }.filter(_ != Node.End)
  }

  def getOptimalConnections(nodes: List[Node]): List[Conn] = {
    getConnections(nodes)
  }

  def getNormalizedConnections(connections: List[Conn]) = {
    connections.groupBy(_.n2).map {
      case (n2, conns2) => n2 -> conns2.map(_.num).sum
    }.map { case (n2, num) => Conn(connections.head.n1, n2, num) }.toList
  }

  def getConnections(nodes: List[Node]): List[Conn] = nodes match {
    case Nil => Nil
    case x :: xs =>
      val connections = getNormalizedConnections(getConnections(x))
      val rest = getRest(xs, connections)
      println("connections = " + connections)
      println("rest = " + rest)
      connections ++ getConnections(rest)
  }

  def getConnections(node: Node): List[Conn] = {
    if (node.needs == 0) Nil
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
