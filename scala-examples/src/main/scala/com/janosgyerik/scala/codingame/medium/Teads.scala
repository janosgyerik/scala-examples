package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

import scala.collection.immutable.IndexedSeq

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

class Node(val id: String) {
  override def toString = s"Node($id)"

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
}

object Teads {

  def solve(scanner: Scanner) = {
    minMaxDistance(parseInput(scanner))
  }

  def parseInput(scanner: Scanner) = {
    val linesCount = scanner.nextInt()
    scanner.nextLine()

    for {_ <- 1 to linesCount} yield {
      val line = scanner.nextLine()
      val parts = line.split(" ").take(2)
      new Link(new Node(parts(0)), new Node(parts(1)))
    }
  }

  def minMaxDistance(links: IndexedSeq[Link]): Int = {
    val nodes = links.map(link => link.n1).toSet ++ links.map(link => link.n2).toSet

    var distance = 0
    while (true) {
      distance = distance + 1
      var index = 0
      while (index < nodes.size) {
        val node = nodes.toList(index)
        if (findNodesWithinDistance(links.toSet, node, distance).size == nodes.size) return distance
        index = index + 1
      }
    }
    0
  }

  def findNodesWithinDistance(links: Set[Link], node: Node, distance: Int) = {
    def findNodesWithinDistance(visited: Set[Node], d: Int): Set[Node] = {
      if (d == 0) visited
      else {
        val neighbors1 = for {link <- links if visited.contains(link.n1) && !visited.contains(link.n2)} yield link.n2
        val neighbors2 = for {link <- links if visited.contains(link.n2) && !visited.contains(link.n1)} yield link.n1
        val neighbors = neighbors1 ++ neighbors2
        val newVisited = visited ++ neighbors
        if (neighbors.nonEmpty) findNodesWithinDistance(newVisited, d - 1)
        else newVisited
      }
    }
    findNodesWithinDistance(Set(node), distance)
  }
}
