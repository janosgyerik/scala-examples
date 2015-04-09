package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

class Node(val id: String) {
  override def toString = s"$id"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class Link(val n1: Node, val n2: Node) {
  override def toString = s"Link($n1, $n2)"

  def swap = new Link(n2, n1)
}

object Teads {

  def solve(scanner: Scanner) = {
    minMaxDistance(parseInput(scanner))
  }

  def parseInput(scanner: Scanner) = {
    val linesCount = scanner.nextInt()
    scanner.nextLine()

    (for {_ <- 1 to linesCount} yield {
      val line = scanner.nextLine()
      val parts = line.split(" ").take(2)
      new Link(new Node(parts(0)), new Node(parts(1)))
    }).toSet
  }

  def mkNeighborMap(links0: Set[Link]) = {
    val reversedLinks = links0.map(_.swap)
    val links = links0 ++ reversedLinks
    val nodePairs = links.map(link => (link.n1, link.n2))
    nodePairs.groupBy(_._1).map { case (n1, pairs) => (n1, pairs.map(_._2)) }
  }

  def minMaxDistance(links: Set[Link]): Int = {
    val neighborMap = mkNeighborMap(links)
    val nodes = neighborMap.toList.sortBy { case (_, neighbors) => neighbors.size }.reverseMap(_._1)

    var distance = 0
    while (true) {
      distance = distance + 1
      var index = 0
      while (index < nodes.size) {
        val node = nodes(index)
        if (findNodesWithinDistance(neighborMap, links.toSet, node, distance).size == nodes.size) return distance
        index = index + 1
      }
    }
    throw new IllegalStateException("unreachable line: all nodes must be found by now")
  }

  type NeighborMap = Map[Node, Set[Node]]

  def findNodesWithinDistance(neighborMap: NeighborMap, links: Set[Link], node: Node, distance: Int) = {
    def findNodesWithinDistance(visited: Set[Node], neighbors: Set[Node], d: Int): Set[Node] = {
      if (d == 0 || neighbors.isEmpty) visited
      else {
        val newVisited = visited ++ neighbors
        val newNeighbors = for {
          node <- neighbors
          neighbor <- neighborMap.get(node).get if !newVisited.contains(neighbor)
        } yield neighbor

        findNodesWithinDistance(newVisited, newNeighbors, d - 1)
      }
    }
    findNodesWithinDistance(Set(node), neighborMap.get(node).get, distance)
  }
}
