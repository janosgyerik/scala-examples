package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

object Scrabble {

  def parseInput(scanner: Scanner) = {
    val wordsCount = scanner.nextInt()
    scanner.nextLine()

    val words = {
      for (_ <- 1 to wordsCount) yield scanner.nextLine()
    }.toSet
    val letters = scanner.nextLine()
    (words, letters)
  }

  def solve(scanner: Scanner): Unit = {
    val (words, letters) = parseInput(scanner)
    val scrabble = new Scrabble(words)
    println(scrabble.findBestWord(letters))
  }

  val scoreMap = Map(
    "eaionrtlsu" -> 1,
    "dg" -> 2,
    "bcmp" -> 3,
    "fhvwy" -> 4,
    "k" -> 5,
    "jx" -> 8,
    "qz" -> 10
  ).flatMap(x => x._1.map(y => y -> x._2))

  def calculateScore(word: String) = word.map(scoreMap).sum
}

class Scrabble(words: Set[String]) {

  def findPossibleWords(letters: String): Set[String] = ???

  def findBestWord(letters: String): String = {
    val possibleWordsWithScores = for {
      word <- findPossibleWords(letters)
    } yield (word, Scrabble.calculateScore(word))
    ""
  }

}
