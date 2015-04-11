package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

object Teads {

  def solve(scanner: Scanner) = {
    minMaxDistance(parseInput(scanner))
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

  def minMaxDistance(links: Set[Link]): Int = {
    val neighbors = mkConnMap(links)

    def countReductionSteps(conn: ConnMap, steps: Int): Int = {
      if (conn.size < 2) steps
      else {
        val nonFinalConn = conn.filter { case (n, nx) => nx.size > 1 }
        val nextConn = nonFinalConn.map { case (n, nx) => n -> nx.filter(nonFinalConn.contains) }
        countReductionSteps(nextConn, steps + 1)
      }
    }
    countReductionSteps(neighbors, 0)
  }
}
