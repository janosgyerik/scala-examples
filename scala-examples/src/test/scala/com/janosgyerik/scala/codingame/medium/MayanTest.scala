package com.janosgyerik.scala.codingame.medium

import java.io.File
import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class MayanTest extends FunSuite {

  import com.janosgyerik.scala.codingame.medium.Mayan._

  val dialect = Dialect.fromScanner(new Scanner(
    "4 4\n" +
      ".oo.o...oo..ooo.oooo....o...oo..ooo.oooo____o...oo..ooo.oooo____o...oo..ooo.oooo\n" +
      "o..o................____________________________________________________________\n" +
      ".oo.........................................____________________________________\n" +
      "................................................................________________"))

  test("dialect width is 4") {
    assert(4 == dialect.width)
  }

  test("dialect height is 4") {
    assert(4 == dialect.height)
  }

  test("dialect digit 7") {
    assert(
      "oo..\n" +
        "____\n" +
        "....\n" +
        "....\n" == dialect.digits(7).toString)
  }

  test("parsing mayan 4805") {
    assert(4805 == Number.fromScanner(dialect, new Scanner(
      "12\n" +
        "oo..\n" +
        "____\n" +
        "____\n" +
        "....\n" +
        ".oo.\n" +
        "o..o\n" +
        ".oo.\n" +
        "....\n" +
        "....\n" +
        "____\n" +
        "....\n" +
        "....\n"
    )).longValue)
  }

  test("parsing mayan 1") {
    assert(1 == Number.fromScanner(dialect, new Scanner(
      "4\n" +
        "o...\n" +
        "....\n" +
        "....\n" +
        "....\n"
    )).longValue)
  }

  test("converting 4805 to mayan") {
    val mayan = dialect.fromLong(4805)
    assert(4805 == mayan.longValue)

    val digits = mayan.digits
    assert(Vector(12, 0, 5) == digits.map(x => x.longValue))
  }

  val testdir = "src/test/resources/codingame/medium/mayan"

  test("input1") {
    assert(Source.fromFile(testdir + "/out1.txt").mkString
      == solve(new Scanner(new File(testdir + "/in1.txt"))).toString)
  }

  for (i <- 2 to 12) {
    test("input" + i) {
      assert(Source.fromFile(testdir + "/out" + i + ".txt").mkString
        == solve(new Scanner(new File(testdir + "/in" + i + ".txt"))).toString)
    }
  }

}
