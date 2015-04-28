package com.janosgyerik.scala.codingame.challenges.nospoon2.level1

import java.util.Scanner

object Player extends App {
  val scanner = new Scanner(System.in)

  solve(scanner)

  def solve(scanner: Scanner): Unit = {
    val width = scanner.nextInt
    val height = scanner.nextInt
    scanner.nextLine

    val lines = { for { _ <- List.range(0, height) } yield scanner.nextLine }.toArray

    import GameState._

    toFormattedLines(parseNodes(lines)).foreach(println)
  }
}

case class Node(rowNum: Int, colNum: Int, right: Node = null, down: Node = null)

case class Link(n1: Node, n2: Node)

object GameState {

  val emptyMarker = '.'

  def parseNodes(lines: Array[String]): List[Node] = {
    val width = lines(0).length
    val height = lines.length

    def findRightNeighbor(node: Node) = {
      val rowNum = node.rowNum
      val row = lines(rowNum)

      def findRightNeighbor(colNum: Int): Node = {
        if (colNum >= width) null
        else if (row(colNum) == emptyMarker) findRightNeighbor(colNum + 1)
        else Node(rowNum, colNum)
      }
      findRightNeighbor(node.colNum + 1)
    }

    def findDownNeighbor(node: Node) = {
      val colNum = node.colNum

      def findDownNeighbor(rowNum: Int): Node = {
        if (rowNum >= height) null
        else if (lines(rowNum)(colNum) == emptyMarker) findDownNeighbor(rowNum + 1)
        else Node(rowNum, colNum)
      }
      findDownNeighbor(node.rowNum + 1)
    }

    def parseNode(rowNum: Int, colNum: Int): Node = {
      val char = lines(rowNum)(colNum)
      if (char == emptyMarker) null
      else {
        val node = Node(rowNum, colNum)
        Node(rowNum, colNum, findRightNeighbor(node), findDownNeighbor(node))
      }
    }
    {
      for {
        rowNum <- List.range(0, lines.length)
        colNum <- List.range(0, lines(0).length)
      } yield parseNode(rowNum, colNum)
    }.filter(_ != null)
  }
  
  def toFormattedLines(nodes: List[Node]) = {
    def formatSingleNode(node: Node) = "%s %s".format(node.colNum, node.rowNum)
    
    def formatNeighbor(node: Node) =
      if (node == null) formatSingleNode(Node(-1, -1))
      else formatSingleNode(node)
      
    def formatNode(node: Node) =
      "%s %s %s".format(formatSingleNode(node), formatNeighbor(node.right), formatNeighbor(node.down))
    
    nodes.map(formatNode)
  }
}
