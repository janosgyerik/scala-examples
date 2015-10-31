package com.janosgyerik.scala.codingame.medium.giants

import java.util.Scanner

import scala.collection.mutable.{Map => MutableMap}

object Solution extends App {
  println(Giants.solve(new Scanner(System.in)))
}

object Giants {
  def solve(input: String): Int = {
    solve(new Scanner(input))
  }

  def solve(scanner: Scanner) = {
    def parseInput() = {
      val n = scanner.nextInt()

      val links = for (i <- 1 to n) yield (scanner.nextInt(), scanner.nextInt())
      links.groupBy(_._1).map { case (n1, pairs) => (n1, pairs.map(_._2).toList) }
    }

    val mapping = parseInput()
    val longestLengthMap: MutableMap[Int, Int] = MutableMap()

    def findLength(node: Int): Int = {
      longestLengthMap.get(node) match {
        case Some(length) => length
        case None =>
          val length = {
            mapping.get(node) match {
              case None => 1
              case Some(neighbors) => neighbors.map(neighbor => 1 + findLength(neighbor)).max
            }
          }
          longestLengthMap.put(node, length)
          length
      }
    }

    mapping.keys.map(findLength).max
  }
}
