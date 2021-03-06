package com.janosgyerik.scala.codingame.medium

import java.io.File
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source


@RunWith(classOf[JUnitRunner])
class ScrabbleTest extends FunSuite {

  import com.janosgyerik.scala.codingame.medium.Scrabble._

  val testdir = "src/test/resources/codingame/medium/scrabble"

  for (i <- 1 to 2) {
    test("input" + i) {
      assert(Source.fromFile(testdir + "/out" + i + ".txt").mkString.trim
        == solve(new Scanner(new File(testdir + "/in" + i + ".txt"))))
    }
  }

  test("parseInput example 1") {
    val input = "5\nbecause\nfirst\nthese\ncould\nwhich\nhicquwh"
    val (words, letters) = parseInput(new Scanner(input))
    assert("hicquwh" == letters)
    assert(Set("because", "first", "these", "could", "which") == words.toSet)

    val scrabble = new Scrabble(words)
    assert("which" == scrabble.wordsWithSortedLetters("chhiw"))
  }

  test("scoreMap") {
    assert(1 == scoreMap('a'))
    assert(3 == scoreMap('b'))
    assert(8 == scoreMap('x'))
    assert(8 == scoreMap('j'))
    assert(10 == scoreMap('q'))
  }

  test("score of 'jack' is 17") {
    assert(8 == scoreMap('j'))
    assert(1 == scoreMap('a'))
    assert(3 == scoreMap('c'))
    assert(5 == scoreMap('k'))
    assert(17 == calculateScore("jack"))
  }

  test("score of 'mike' is 10") {
    assert(3 == scoreMap('m'))
    assert(1 == scoreMap('i'))
    assert(5 == scoreMap('k'))
    assert(1 == scoreMap('e'))
    assert(10 == calculateScore("mike"))
  }

  test("getWordWithBestScore in apple, jack, mike is jack") {
    assert("jack" == getWordWithBestScore(List("apple", "jack", "mike")))
    assert("mike" == getWordWithBestScore(List("apple", "mike")))
  }

  test("selectLettersUsingBitmask apple 14") {
    assert("ppl" == selectLettersUsingBitmask("apple", 14))
  }

  test("selectLettersUsingBitmask apple 19") {
    assert("ale" == selectLettersUsingBitmask("apple", 19))
  }

  test("getPossibleLetterSelections: ale") {
    assert(Vector("e", "l", "le", "a", "ae", "al", "ale") == getPossibleLetterSelections("ale"))
  }

  test("findPossibleWords in ale, pale, pole, jack using apple") {
    val words = List("ale", "pale", "pole", "jack")
    val letters = "apple"

    val scrabble = new Scrabble(words)
    assert(Set("ale", "pale") == scrabble.findPossibleWords(letters))
  }

}
