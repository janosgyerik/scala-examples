package com.janosgyerik.scala.codingame.challenges.skynet

import java.util.Scanner

object Player extends App {
  val scanner = new Scanner(System.in)
  val initialInputs = InitialInputs.fromScanner(scanner)
  var game = new Game(initialInputs)
  while (scanner.hasNext) {
    val roundInput = RoundInputs.fromScanner(scanner)
    val (nextMove, nextGame) = game.next(roundInput)
    println(nextMove)
    game = nextGame
  }
}

object InitialInputs {
  def fromString(string: String) = fromScanner(new Scanner(string))

  def fromScanner(scanner: Scanner): InitialInputs = {
    val nodeCount = scanner.nextInt
    val linkCount = scanner.nextInt
    val gwCount = scanner.nextInt

    val nodeIdPairs = for { _ <- List.range(0, linkCount) } yield (scanner.nextInt, scanner.nextInt)
    val gwIds = for { _ <- List.range(0, gwCount) } yield scanner.nextInt

    new InitialInputs(nodeIdPairs, gwIds)
  }
}

class InitialInputs(val nodeIdPairs: List[(Int, Int)], val gwIds: List[Int]) {

  def -(link: Link) = {
    val pairs = nodeIdPairs.filter(pair => nonMatching(pair, link))
    new InitialInputs(pairs, gwIds)
  }

  def nonMatching(pair: (Int, Int), link: Link): Boolean = pair match {
    case (id1, id2) => !(id1 == link.id1 && id2 == link.id2 || id1 == link.id2 && id2 == link.id1)
  }
}

object RoundInputs {
  def fromString(string: String) = fromScanner(new Scanner(string))

  def fromScanner(scanner: Scanner): RoundInputs = new RoundInputs(scanner.nextInt)
}

class RoundInputs(val agentNodeId: Int)

case class Node(id: Int)

case class Link(id1: Int, id2: Int) {
  val n1 = Node(id1)
  val n2 = Node(id2)
}

class Game(initialInputs: InitialInputs) {
  val allNodes = initialInputs.nodeIdPairs.flatMap {
    case (n1, n2) => List(Node(n1), Node(n2))
  }.toSet

  val allLinks = initialInputs.nodeIdPairs.flatMap {
    case (n1, n2) => List(Link(n1, n2), Link(n2, n1))
  }.toSet

  val gwNodes = initialInputs.gwIds.map(Node).toSet

  val nodeLinksToGw = allLinks.filter(link => gwNodes.contains(link.n2))

  def getLinkToDelete(inputs: RoundInputs): Link = {
    val primary = nodeLinksToGw.filter(link => link.id1 == inputs.agentNodeId)
    if (primary.nonEmpty) primary.head
    else nodeLinksToGw.head
  }

  def next(inputs: RoundInputs) = {
    val link = getLinkToDelete(inputs)
    val nextMove = s"${link.id1} ${link.id2}"
    (nextMove, new Game(initialInputs - link))
  }

}
