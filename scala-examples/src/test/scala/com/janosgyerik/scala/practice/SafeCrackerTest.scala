package com.janosgyerik.scala.practice

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SafeCrackerTest extends FunSuite {
  import com.janosgyerik.scala.practice.SafeCracker._

  test("test perm 0") {
    assert(List() == perm(0))
  }

  test("test perm 1") {
    assert(10 == perm(1).size)
  }

  test("test perm 2") {
    assert(100 == perm(2).size)
  }

  test("test perm 2 / 0") {
    assert(List(0, 0) == perm(2).head)
  }

  test("test perm 2 / 33") {
    assert(List(3, 3) == perm(2)(33))
  }

  test("test perm 2 / 58") {
    assert(List(5, 8) == perm(2)(58))
  }

  test("test perm 3 / 279") {
    assert(List(2, 7, 9) == perm(3)(279))
  }
}
