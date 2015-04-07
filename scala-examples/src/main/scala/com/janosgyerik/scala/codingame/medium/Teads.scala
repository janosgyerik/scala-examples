package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

import scala.collection.immutable.IndexedSeq

object Solution extends App {
  val answer = Teads.solve(new Scanner(System.in))
  println(answer)
}

class Node(id: Int, val neighbors: Set[Node]) {

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
      (parts(0), parts(1))
    }
  }

  def minMaxDistance(tuples: IndexedSeq[(String, String)]) = ???

  def distance(n1: Node, n2: Node) = ???
}
