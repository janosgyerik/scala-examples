package com.janosgyerik.scala.codingame.easy

object SkynetChasm {

  abstract class Action

  case object Speed extends Action
  case object Wait extends Action
  case object Slow extends Action
  case object Jump extends Action

  trait State {
    def next(action: Action): State = throw new IllegalAccessError("This is a terminating state")
  }

  case object Dead extends State
  case object Landed extends State

  case class Running(game: Game, speed: Int, pos: Int = 0) extends State {
    require(posOnRunway(pos) || posOnLanding(pos))

    def posOnRunway(pos: Int) =
      0 <= pos && pos < game.gapStart

    def posOnLanding(pos: Int) =
      game.landingStart <= pos && pos < game.landingEnd

    override def next(action: Action): State = {
      val newSpeed = action match {
        case Speed => speed + 1
        case Slow => speed - 1
        case _ => speed
      }

      val newPos = pos + newSpeed

      val wasOnRunway = posOnRunway(pos)
      val wasOnLanding = posOnLanding(pos)
      val onRunway = posOnRunway(newPos)
      val onLanding = posOnLanding(newPos)
      val stopped = newSpeed == 0
      val onLandingSlowingDown = wasOnLanding && onLanding && !stopped
      val jumpedOverGap = wasOnRunway && onLanding && action == Jump

      if (onRunway || onLandingSlowingDown || jumpedOverGap) Running(game, newSpeed, newPos)
      else if (onLanding && stopped) Landed
      else Dead
    }
  }

  class Game(val initialSpeed: Int, val gapStart: Int, val landingStart: Int, val landingEnd: Int) {
    val initialState: State = Running(this, initialSpeed)
  }

//  def findSuccessfulActionSequence(params: GameParams): List[Action] = {
//    def findSuccessfulActionSequence(list: List[Action], state: BikeState): List[Action] = {
//      if (state.landed) list
//      else if (!state.alive) List()
//      else findSuccessfulActionSequenceFromState(list, state)
//    }
//
//    def findSuccessfulActionSequenceFromState(list: List[Action], state: BikeState): List[Action] = {
//      val possibleActions = getPossibleActions(params, state)
//      var index = 0
//      while (index < possibleActions.size) {
//        val action = possibleActions(index)
//        val newState = applyAction(params, state, action)
//        val successfulActionSequence = findSuccessfulActionSequence(action :: list, newState)
//        if (successfulActionSequence.nonEmpty) return successfulActionSequence.reverse
//        index = index + 1
//      }
//      List()
//    }
//
//    val initialState = getInitialState(params)
//
//    findSuccessfulActionSequenceFromState(List(), initialState)
//  }
//
//  def isBeforeGap(params: GameParams, state: BikeState): Boolean =
//    state.pos < params.gapStart
//
//  def isGapImminent(params: GameParams, state: BikeState): Boolean =
//    state.pos + state.speed >= params.gapStart
//
//  def getCommonReasonableActions(state: BikeState): List[Action] = {
//    List(Speed(), Wait()) ++ { if (state.speed > 1) List(Slow()) else Nil }
//  }
//
//  def getPossibleActions(params: GameParams, state: BikeState): List[Action] = {
//    if (isBeforeGap(params, state)) {
//      if (state.speed == 0) List(Speed())
//      else if (isGapImminent(params, state)) List(Jump())
//      else getCommonReasonableActions(state)
//    } else {
////      List(Slow())
//      getCommonReasonableActions(state)
//    }
//  }
//
//  def getInitialState(params: GameParams) = new BikeState(params.initialSpeed)
}
