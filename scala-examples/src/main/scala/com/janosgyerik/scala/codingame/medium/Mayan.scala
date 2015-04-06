package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

object Mayan {

  class Dialect(val width: Int, val height: Int, val digits: IndexedSeq[Digit]) {

    val lines2digits = digits.map(x => x.lines -> x).toMap

    def numberFromLines(lines: IndexedSeq[String]) = {
      val digits =
        for {start <- 0 until (lines.size, height)}
          yield digitFromLines(lines.slice(start, start + height))
      new Number(digits)
    }

    def digitFromLines(lines: IndexedSeq[String]) = {
      lines2digits.get(lines).get
    }

    def fromInt(intValue: Int) = {
      def inner(num: Int): List[Digit] = num match {
        case 0 => List()
        case x => digits(num % Dialect.radix) :: inner(num / Dialect.radix)
      }
      new Number(inner(intValue).toIndexedSeq)
    }
  }

  object Dialect {
    val radix = 20

    def fromScanner(scanner: Scanner) = {
      val width = scanner.nextInt()
      val height = scanner.nextInt()
      scanner.nextLine()

      val lines = for {_ <- 1 to height} yield scanner.nextLine()

      def extractDigitLines(intValue: Int) = {
        val start = width * intValue
        for {row <- 0 until height} yield lines(row).slice(start, start + width)
      }

      val digits = for {intValue <- 0 to radix} yield new Digit(extractDigitLines(intValue), intValue)

      new Dialect(width, height, digits)
    }
  }

  class Digit(val lines: IndexedSeq[String], val intValue: Int) {
    override def toString = s"Digit($lines, $intValue)"
  }

  class Number(digits: IndexedSeq[Digit]) {
    lazy val intValue: Int = digits.foldLeft(0)((x, y) => Dialect.radix * x + y.intValue)
  }

  object Number {
    def fromScanner(dialect: Dialect, scanner: Scanner) = {
      val linesCount = scanner.nextInt()
      scanner.nextLine()
      val lines = for {_ <- 1 to linesCount} yield scanner.nextLine()
      dialect.numberFromLines(lines)
    }
  }

  def solve(scanner: Scanner) = {
    val scanner = new Scanner("")

    val dialect = Dialect.fromScanner(scanner)

    val n1 = Number.fromScanner(dialect, scanner).intValue
    val n2 = Number.fromScanner(dialect, scanner).intValue

    val op = scanner.next()
    val result = op match {
      case "+" => n1 + n2
      case "-" => n1 - n2
      case "*" => n1 * n2
      case "/" => n1 / n2
    }

    dialect.fromInt(result)
  }
}
