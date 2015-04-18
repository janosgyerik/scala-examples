package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

object Scrabble {

  def parseInput(scanner: Scanner) = {
    val wordsCount = scanner.nextInt()
    scanner.nextLine()

    val words = { for (_ <- 1 to wordsCount) yield scanner.nextLine() }.toSet
    val letters = scanner.nextLine()
    (words, letters)
  }

  def findBestWord(words: Set[String], letters: String): Any = ???

  def solve(scanner: Scanner): Unit = {
    val (words, letters) = parseInput(scanner)
    println(findBestWord(words, letters))
  }

}
