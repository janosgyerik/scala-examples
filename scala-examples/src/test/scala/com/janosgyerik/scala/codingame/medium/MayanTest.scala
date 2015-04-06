package com.janosgyerik.scala.codingame.medium

import java.util.Scanner

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

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
    assert("Digit(Vector(oo.., ____, ...., ....), 7)" == dialect.digits(7).toString)
  }

  test("dialect digit 12") {
    assert("Digit(Vector(oo.., ____, ____, ....), 12)" == dialect.digits(12).toString)
  }

  test("dialect digit 0") {
    assert("Digit(Vector(.oo., o..o, .oo., ....), 0)" == dialect.digits.head.toString)
  }

  test("dialect digit 5") {
    assert("Digit(Vector(...., ____, ...., ....), 5)" == dialect.digits(5).toString)
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
    )).intValue)
  }

  test("parsing mayan 1") {
    assert(1 == Number.fromScanner(dialect, new Scanner(
      "4\n" +
        "o...\n" +
        "....\n" +
        "....\n" +
        "....\n"
    )).intValue)
  }

  test("converting 4805 to mayan") {
    val mayan = dialect.fromInt(4805)
    assert(4805 == mayan.intValue)

    val digits = mayan.digits
    assert(Vector(12, 0, 5) == digits.map(x => x.intValue))
  }

}
