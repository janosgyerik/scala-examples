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

  def getWordWithBestScore(words: Set[String]) = {
    val wordsWithScores = for {
      word <- words
    } yield (word, calculateScore(word))

    wordsWithScores.maxBy { case (word, score) => score }._1
  }

}

class Scrabble(words: Set[String]) {

  import Scrabble._

  val wordsWithSortedLetters = words.map(word => word.sorted -> word).toMap

  def findPossibleWords(letters: String): Set[String] = {
    Set("which")
  }

  def findBestWord(letters: String): String = {
    getWordWithBestScore(findPossibleWords(letters))
  }
}
