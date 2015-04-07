package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

import scala.collection.immutable.IndexedSeq

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

//class Node(id: Int, val neighbors: Set[Node]) {
//
//}

object Teads {

  type Node = String

  class Link(val n1: Node, val n2: Node, val distance: Int) {
    override def toString = s"Link($n1, $n2, $distance)"
  }

  def solve(scanner: Scanner) = {
    minMaxDistance(parseInput(scanner))
  }

  def parseInput(scanner: Scanner) = {
    val linesCount = scanner.nextInt()
    scanner.nextLine()

    val nodes = for {_ <- 1 to linesCount} yield {
      val line = scanner.nextLine()
      val parts = line.split(" ").take(2)
      (parts(0), parts(1))
    }
    { nodes ++ nodes.map(x => x.swap) } map(x => new Link(x._1, x._2, 1))
  }

  def minMaxDistance(links: IndexedSeq[Link]) = ???

  def distance(n1: Node, n2: Node) = ???
}
