package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

object Teads {

  def solve(scanner: Scanner, verbose: Boolean = false) = {
    minMaxDistance(parseInput(scanner), verbose)
  }

  case class Node(id: String) {
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

  case class Link(n1: Node, n2: Node) {
    override def toString = s"Link($n1, $n2)"

    def swap = new Link(n2, n1)
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

  type ConnMap = Map[Node, Set[Node]]

  def mkConnMap(links0: Set[Link]) = {
    val reversedLinks = links0.map(_.swap)
    val links = links0 ++ reversedLinks
    val nodePairs = links.map(link => (link.n1, link.n2))
    nodePairs.groupBy(_._1).map { case (n1, pairs) => (n1, pairs.map(_._2)) }
  }

  def mergeConnMaps(m1: ConnMap, m2: ConnMap) = {
    def flatten(m: ConnMap) =
      m.toList.flatMap(x => for {n2 <- x._2} yield (x._1, n2))

    (flatten(m1) ++ flatten(m2)).groupBy(_._1).map { case (n, list) => n -> list.map(_._2).toSet }
  }

  def minMaxDistance(links: Set[Link], verbose: Boolean = false): Int = {
    val neighbors = mkConnMap(links)
//    val nodes = neighbors.toList.sortBy { case (_, neighbors) => neighbors.size }.reverseMap(_._1)
    val nodes = links.flatMap { case link => List(link.n1, link.n2) }

    def inner(conn: ConnMap, explore: ConnMap, d: Int): Int = {
      if (verbose) printStats(conn, explore, d)

      val nextConn = mergeConnMaps(conn, explore)

      if (fullReachExists(nextConn)) d + 1
      else {
        val nextExplore = getNextExplore(conn, explore)
        inner(nextConn, nextExplore, d + 1)
      }
    }

    def printStats(conn: ConnMap, explore: ConnMap, d: Int): Unit = {
      println("nodes: %d".format(nodes.size))
      println("depth: %d".format(d))
      printConnMapStats("conn", conn)
      printConnMapStats("explore", explore)
      println("---")
    }

    def printConnMapStats(name: String, map: ConnMap): Unit = {
      println("%s min: %d".format(name, map.values.map(_.size).min))
      println("%s max: %d".format(name, map.values.map(_.size).max))
      println("%s avg: %s".format(name, map.values.map(_.size).sum / 1.0 / nodes.size))
    }

    def getNextExplore(conn: ConnMap, explore: ConnMap) = {
      val links = for {
        n <- nodes
        x <- explore.get(n).get
        n2 <- neighbors.get(x).get if !conn.get(n).get.contains(n2)
      } yield Link(n, n2)
      mkConnMap(links)
    }

    def fullReachExists(conn: ConnMap) =
      conn.values.map(_.size).toSet.contains(nodes.size)

    val selfMap = nodes.map(x => x -> Set(x)).toMap

    inner(selfMap, neighbors, 0)
  }

  def findNodesWithinDistance(neighborMap: ConnMap, links: Set[Link], node: Node, distance: Int) = {
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
