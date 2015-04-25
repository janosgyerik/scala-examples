package com.janosgyerik.scala.codingame.challenges.challenge

import java.util.Scanner

object Player extends App {
  val scanner = new Scanner(System.in)

  solve(scanner)

  def solve(scanner: Scanner): Unit = {
    val width = scanner.nextInt
    val height = scanner.nextInt
    scanner.nextLine

    val lines = { for { _ <- List.range(0, height) } yield scanner.nextLine }.toArray

    val links = Solution.parseLinks(lines)
    val nodes = Solution.getNodes(links)

    var game = new GameState(links, nodes)
    do {
      val nodes = game.findNodesThatNeedAllPossibleConnections
      if (nodes.nonEmpty) {
        val node = nodes.head
        val connections = game.getAllPossibleConnections(node)
        connections.foreach {
          case (link, n) =>
            println("%s %s %s %s %s".format(link.n1.col, link.n1.row, link.n2.col, link.n2.row, n))
        }
        game = game.removeConnections(connections)
      }
    } while (game.nodes.nonEmpty)
  }
}

case class Node(row: Int, col: Int, needs: Int) {

  def findNeighbors(links: List[Link]): Set[Node] =
    links.filter(_.contains(this)).map(link => if (link.n1 == this) link.n2 else link.n1).toSet

  def needsAllPossibleConnections(links: List[Link]): Boolean = {
    findNeighbors(links).toList.map(_.getProvidableCount).sum == needs
  }
  
  def getProvidableCount = math.min(needs, 2)
}

case class Link(n1: Node, n2: Node) {
  def contains(node: Node): Boolean = n1 == node || n2 == node

}

class GameState(val links: List[Link], val nodes: Set[Node]) {
  def findNodesThatNeedAllPossibleConnections = {
    nodes.filter(_.needsAllPossibleConnections(links))
  }
  
  def getAllPossibleConnections(node: Node): Set[(Link, Int)] = {
    for {
      neighbor <- node.findNeighbors(links)
    } yield (Link(node, neighbor), neighbor.getProvidableCount)
  }

  def computeNeedReductions(connections: Set[(Link, Int)]) = {
    connections.toList.flatMap {
      case (link, n) => List((link.n1, n), (link.n2, n))
    }.groupBy(_._1).map { case (node, pairs) => (node, pairs.map(_._2).sum) }
  }

  def computeNodeReductionMap(reductions: Map[Node, Int]) = {
    for {
      (k, v) <- reductions
    } yield k -> Node(k.row, k.col, k.needs - v)
  }

  def computeNodeUpdates(connections: Set[(Link, Int)]): (Set[Node], Set[Node], Set[Node]) = {
    val updates = computeNodeReductionMap(computeNeedReductions(connections))
    val nodesUsed = updates.keys.toSet
    val nodesToRemove = updates.values.filter(_.needs == 0).toSet
    val nodesToUpdate = updates.values.filter(_.needs > 0).toSet
    (nodesUsed, nodesToRemove, nodesToUpdate)
  }

  def removeConnections(list: Set[(Link, Int)]): GameState = {
    val (nodesUsed, nodesToRemove, nodesToUpdate) = computeNodeUpdates(list)
    val newNodes = nodes.filter(node => !nodesUsed.contains(node)) ++ nodesToUpdate
    val newLinks = links.filter(link => !nodesToRemove.contains(link.n1) && !nodesToRemove.contains(link.n2))
    new GameState(newLinks, newNodes)
  }
}

object Solution {

  val emptyMarker = '.'

  def getNodes(links: List[Link]): Set[Node] = {
    links.flatMap(link => List(link.n1, link.n2)).toSet
  }

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
