package com.janosgyerik.scala.codingame.easy

object SkynetChasm {

  abstract class Action

  case class Speed() extends Action
  case class Wait() extends Action
  case class Slow() extends Action
  case class Jump() extends Action

  class GameParams(val initialSpeed: Int, val gapStart: Int, val landingStart: Int, val landingEnd: Int)

  class BikeState(val speed: Int, val pos: Int = 0, val landed: Boolean = false, val alive: Boolean = true)

  def applyAction(params: GameParams, state: BikeState, action: Action) = {
    val newSpeed = action match {
      case Speed() => state.speed + 1
      case Slow() => state.speed - 1
      case _ => state.speed
    }

    val newPos = state.pos + newSpeed

    val onLanding = newPos >= params.landingStart && newPos <= params.landingEnd

    val landed = onLanding && newSpeed == 0

    val alive = onLanding || newPos < params.gapStart

    new BikeState(newSpeed, newPos, landed, alive)
  }

  def findSuccessfulActionSequence(params: GameParams): List[Action] = {
    def findSuccessfulActionSequence(list: List[Action], state: BikeState): List[Action] = {
      if (state.landed) list
      else if (!state.alive) List()
      else findSuccessfulActionSequenceFromState(list, state)
    }

    def findSuccessfulActionSequenceFromState(list: List[Action], state: BikeState): List[Action] = {
      val possibleActions = getPossibleActions(params, state)
      var index = 0
      while (index < possibleActions.size) {
        val action = possibleActions(index)
        val newState = applyAction(params, state, action)
        val successfulActionSequence = findSuccessfulActionSequence(action :: list, newState)
        if (successfulActionSequence.nonEmpty) return successfulActionSequence.reverse
        index = index + 1
      }
      List()
    }

    val initialState = getInitialState(params)

    findSuccessfulActionSequenceFromState(List(), initialState)
  }

  def getPossibleActions(params: GameParams, state: BikeState) = {
    if (state.pos < params.gapStart) {
      if (state.speed == 0) List(Speed())
      else if (state.pos + state.speed >= params.gapStart) List(Jump())
      else List(Speed(), Slow(), Wait(), Jump())
    }
    else List(Speed(), Slow(), Wait(), Jump())
  }

  def getInitialState(params: GameParams) = new BikeState(params.initialSpeed)
}
