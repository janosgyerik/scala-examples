package com.janosgyerik.scala.codingame.medium.giants

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class GiantsTest extends FunSuite {

  test("Example 1") {
    assert(3 == Giants.solve("3\n1 2\n1 3\n3 4"))
  }

  test("Example 2") {
    assert(4 == Giants.solve("8\n1 2\n1 3\n3 4\n2 4\n2 5\n10 11\n10 1\n10 3"))
  }

  test("Example 3") {
    assert(3 == Giants.solve("4\n2 3\n8 9\n1 2\n6 3"))
  }

}
