package com.janosgyerik.scala.codingame.medium

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
    assert("Digit(Vector(oo.., ____, ...., ....), 7)" == dialect.digits(7).toString)
  }

}
