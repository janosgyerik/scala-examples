package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

import scala.collection.immutable.IndexedSeq

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

class Node(val id: String) {
  override def toString = s"Node($id)"
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
    val nodes = links.map(link => link.n1).toSet

    //    val selfLinks = nodes.map(node => new Link(node, node, 0))

    def mkDistanceMap(): Map[Link, Int] = {
      val selfLinks = nodes.map(x => new Link(x, x) -> 0)
      val links1 = links.map(x => x -> 1)
      val links2 = links.map(x => new Link(x.n2, x.n1) -> 1)
      val initial = (selfLinks ++ links1 ++ links2).toMap
      initial
    }

//    val distanceMap = mkDistanceMap()
//
//    def distance(n1: Node, n2: Node) = distanceMap.get((n1, n2)).get
//
//    def maxDistance(node: Node) =
//      nodes.filter(x => x != node).map(x => distance(node, x)).max

    // TODO: alternate algorithm:
    // for {distance <- 1 to nodes.size; n <- nodes; if (allNodesReachable(n, distance)) return distance}

//    nodes.map(node => maxDistance(node)).min

    def countReachableNodes(node: Node, i: Int): Int = ???

    for (distance <- 1 to nodes.size) {
      for (node <- nodes) {
        if (countReachableNodes(node, distance) == nodes.size) return distance
      }
    }
    0
  }
}
