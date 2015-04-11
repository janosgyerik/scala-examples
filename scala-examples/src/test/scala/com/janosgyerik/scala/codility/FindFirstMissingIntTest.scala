package com.janosgyerik.scala.codility

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class FindFirstMissingIntTest extends FunSuite {

  import com.janosgyerik.scala.codility.FindFirstMissingInt._

  test("in 1 2 1 1 3 5 -> 4") {
    assert(4 == findFirstMissingInt(Array(1, 2, 1, 1, 3, 5)))
  }

  test("in 1 1 1 -> 2") {
    assert(2 == findFirstMissingInt(Array(1, 1, 1)))
  }

  test("in 1 2 3 4 -> 5") {
    assert(5 == findFirstMissingInt(Array(1, 2, 3, 4)))
  }
}
