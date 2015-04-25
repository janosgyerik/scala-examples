package com.janosgyerik.scala.codingame.challenges.challenge

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class EasyTest extends FunSuite {

  test("") {
    assert("List(Link(Node(0,0,1),Node(2,0,2)), Link(Node(2,0,2),Node(0,0,1)), Link(Node(2,0,2),Node(2,2,1)), Link(Node(2,2,1),Node(2,0,2)))"
      == Solution.parseLinks(Array("1.2", "...", "..1")).toString)
  }
}
