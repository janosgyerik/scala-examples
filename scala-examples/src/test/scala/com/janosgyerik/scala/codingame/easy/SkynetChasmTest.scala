package com.janosgyerik.scala.codingame.easy

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class SkynetChasmTest extends FunSuite {

  import com.janosgyerik.scala.codingame.easy.SkynetChasm._

  val params = new GameParams(0, 20, 22, 100)

  test("initial state") {
    val state = getInitialState(params)
    assert(0 == state.pos)
    assert(params.initialSpeed == state.speed)
    assert(state.alive)
    assert(!state.landed)
  }

  test("apply action Speed at pos 0") {
    val state = getInitialState(params)
    val newState = applyAction(params, state, Speed())
    assert(state.speed + 1 == newState.speed)
  }

  test("possible action at speed 0 before gap is Speed") {
    assert(List(Speed()) == getPossibleActions(params, new BikeState(0, 0)))
  }

  test("possible action at gap - speed is Jump") {
    assert(List(Jump()) == getPossibleActions(params, new BikeState(1, params.gapStart - 1)))
  }

  test("successful action sequence") {
    val sequence = findSuccessfulActionSequence(params)
//    assert("" == sequence.toString())
  }
}
