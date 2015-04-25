package com.janosgyerik.scala.codingame.challenges.challenge

import java.util.Scanner

object Player extends App {
  val scanner = new Scanner(System.in)
  val width = scanner.nextInt
  val height = scanner.nextInt

  val lines = { for { _ <- List.range(1, height) } yield scanner.nextLine }.toArray
  lines.foreach(System.err.println)

  var links = Solution.parseLinks(lines)

  println("0 0 2 0 1")
}

case class Node(row: Int, col: Int, needs: Int) {

}

/**
 * Possible link. The ordering of the nodes doesn't matter.
 *
 * @param n1 node 1
 * @param n2 node 2
 */
case class Link(n1: Node, n2: Node) {

}

object Solution {
  val emptyMarker = '.'

  def parseLinks(lines: Array[String]): List[Link] = {
    val width = lines(0).length
    val height = lines.length

    def findRightNeighbor(node: Node) = {
      val rowNum = node.row
      val row = lines(rowNum)

      def findRightNeighbor(colNum: Int): Node = {
        if (colNum >= width) null
        else if (row(colNum) == emptyMarker) findRightNeighbor(colNum + 1)
        else Node(colNum, rowNum, lines(rowNum)(colNum) - '0')
      }
      findRightNeighbor(node.col + 1)
    }

    def findDownNeighbor(node: Node) = {
      val colNum = node.col

      def findDownNeighbor(rowNum: Int): Node = {
        if (rowNum >= height) null
        else if (lines(rowNum)(colNum) == emptyMarker) findDownNeighbor(rowNum + 1)
        else Node(colNum, rowNum, lines(rowNum)(colNum) - '0')
      }
      findDownNeighbor(node.row + 1)
    }

    def createLinks(n0: Node, n1: Node, n2: Node) = {
      val nodes = List(n0, n1, n2).filter(_ != null)
      for {
        n1 <- nodes
        n2 <- nodes if n1 != n2
      } yield Link(n1, n2)
    }

    def parseLinks(row: Int, col: Int): List[Link] = {
      val char = lines(row)(col)
      if (char == emptyMarker) Nil
      else {
        val needs = char - '0'
        val node = Node(col, row, needs)
        createLinks(node, findRightNeighbor(node), findDownNeighbor(node))
      }
    }
    {
      for {
        row <- List.range(0, lines.length)
        col <- List.range(0, lines(0).length)
      } yield parseLinks(row, col)
    }.flatten
  }

  // output example: x1 y1 x2 y2 n  # add n connections between x1 y1 and x2 y2

  // add the connections that are necessary:
  // - nodes that need all the connections thye can get
  // - 1-nodes with only one neighbor
  // => remove the used connections from the pool

}
