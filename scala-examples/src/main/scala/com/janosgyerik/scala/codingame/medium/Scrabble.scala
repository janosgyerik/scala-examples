package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

import scala.annotation.tailrec

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

  def selectLettersUsingBitmask(letters: String, bitmask: Int) = {
    @tailrec
    def selectLettersUsingBitmask(remainingLetters: String, remainingBitmask: Int, acc: String): String = {
      if (remainingLetters.isEmpty || remainingBitmask == 0) acc
      else {
        val newChar = if ((remainingBitmask & 1) == 1) remainingLetters.last else ""
        selectLettersUsingBitmask(remainingLetters.init, remainingBitmask >> 1, newChar + acc)
      }
    }
    selectLettersUsingBitmask(letters, bitmask, "")
  }

  def getPossibleLetterSelections(letters: String) = {
    assert(letters.length <= 7)
    assert(letters.length > 0)
    for {
      bitmask <- 1 to math.pow(2, letters.length).asInstanceOf[Int]
    } yield selectLettersUsingBitmask(letters, bitmask)
  }

}

class Scrabble(words: Set[String]) {

  import Scrabble._

  val wordsWithSortedLetters = words.map(word => word.sorted -> word).toMap

  def findPossibleWords(letters: String): Set[String] = {
    val sortedLetters = letters.sorted

    val possibleLetterSelections = getPossibleLetterSelections(sortedLetters)

    Set("which")
  }

  def findBestWord(letters: String): String = {
    getWordWithBestScore(findPossibleWords(letters))
  }
}
