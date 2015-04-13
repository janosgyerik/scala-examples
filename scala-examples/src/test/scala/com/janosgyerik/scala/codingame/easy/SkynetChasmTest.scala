package com.janosgyerik.scala.codingame.easy

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class SkynetChasmTest extends FunSuite {

  import com.janosgyerik.scala.codingame.easy.SkynetChasm._

  val gapStart = 30
  val landingStart = 35
  val landingEnd = 100
  val game = new Game(0, gapStart, landingStart, landingEnd)

  test("Running: invalid if before landing start") {
    intercept[IllegalArgumentException](Running(game, 0, -1))
  }

  test("Running: invalid if after landing") {
    intercept[IllegalArgumentException](Running(game, 0, landingEnd + 1))
  }

  test("Running: invalid if on gap start") {
    intercept[IllegalArgumentException](Running(game, 0, gapStart))
  }

  test("Running: invalid if in the gap") {
    intercept[IllegalArgumentException](Running(game, 0, gapStart + 1))
  }

  test("Running: invalid if right before landing start") {
    intercept[IllegalArgumentException](Running(game, 0, landingStart - 1))
  }

  test("Running: valid sanity test") {
    Running(game, 0)
  }

  test("Dead: if fail to jump when gap is imminent") {
    val state = Running(game, landingStart - gapStart + 1, gapStart - 1)
    assert(Dead === state.next(Speed))
    assert(Dead === state.next(Wait))
    assert(Dead === state.next(Slow))
    assert(Dead !== state.next(Jump))
  }

  test("Dead: in front of the gap and too slow") {
    val state = Running(game, landingStart - gapStart, gapStart - 1)
    assert(Dead === state.next(Speed))
    assert(Dead === state.next(Wait))
    assert(Dead === state.next(Slow))
    assert(Dead === state.next(Jump))
  }

  test("Landed: on landing and stopped") {
    val state = Running(game, 1, landingStart)
    assert(Landed === state.next(Slow))
  }

  test("Landed: on landing and slow down in 2 steps") {
    val state = Running(game, 2, landingStart)
    assert(Landed !== state.next(Slow))
    assert(Landed === state.next(Slow).next(Slow))
  }

  test("Dead: on landing but too fast to stop") {
    val state = Running(game, 5, landingEnd - 10)
    assert(Dead !== state.next(Slow))
    assert(Dead !== state.next(Slow).next(Slow))
    assert(Dead !== state.next(Slow).next(Slow).next(Slow))
    assert(Dead === state.next(Slow).next(Slow).next(Slow).next(Slow))
  }

  test("Illegal to Wait/Jump/Slow when speed is 0") {
    intercept[IllegalArgumentException](Running(game, 0).next(Wait))
    intercept[IllegalArgumentException](Running(game, 0).next(Jump))
    intercept[IllegalArgumentException](Running(game, 0).next(Slow))
  }

  test("successful action sequence") {
    val sequence = game.findSuccessfulActionSequence(Running(game, 6))
    assert("" == sequence.toString())
  }
}
