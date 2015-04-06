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

    def fromLong(longValue: Long) = {
      def inner(num: Long): List[Digit] = num match {
        case 0 => List()
        case x => digits((num % Dialect.radix).asInstanceOf[Int]) :: inner(num / Dialect.radix)
      }
      val seq = inner(longValue).toIndexedSeq.reverse
      if (seq.isEmpty) new Number(IndexedSeq(digits.head))
      else new Number(seq)
    }
  }

  object Dialect {
    val radix = 20

    def fromScanner(scanner: Scanner) = {
      val width = scanner.nextInt()
      val height = scanner.nextInt()
      scanner.nextLine()

      val lines = for {_ <- 1 to height} yield scanner.nextLine()

      def extractDigitLines(ord: Int) = {
        val start = width * ord
        for {row <- 0 until height} yield lines(row).slice(start, start + width)
      }

      val digits = for {ord <- 0 to radix} yield new Digit(extractDigitLines(ord), ord)

      new Dialect(width, height, digits)
    }
  }

  class Digit(val lines: IndexedSeq[String], val longValue: Long) {
    override def toString = lines.foldLeft("")((x, y) => x + y + "\n")
  }

  class Number(val digits: IndexedSeq[Digit]) {
    lazy val longValue: Long = digits.foldLeft(0L)((x, y) => Dialect.radix * x + y.longValue)

    override def toString = digits.foldLeft("")((x, y) => x + y)
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
    val dialect = Dialect.fromScanner(scanner)

    val n1 = Number.fromScanner(dialect, scanner).longValue
    val n2 = Number.fromScanner(dialect, scanner).longValue

    val op = scanner.next()
    val result = op match {
      case "+" => n1 + n2
      case "-" => n1 - n2
      case "*" => n1 * n2
      case "/" => n1 / n2
    }

    dialect.fromLong(result)
  }
}
