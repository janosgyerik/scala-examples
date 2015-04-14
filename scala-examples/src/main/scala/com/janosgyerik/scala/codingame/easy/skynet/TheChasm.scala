package com.janosgyerik.scala.codingame.easy.skynet

object Player extends App {
  import TheChasm._

  val r = readInt // the length of the road before the gap.
  val g = readInt // the length of the gap.
  val l = readInt // the length of the landing platform.

  val gapStart = r
  val landingStart = r + g
  val landingEnd = landingStart + l

  // game loop
  //while(true) {
  val s = readInt // the motorbike's speed.
  val x = readInt // the position on the road of the motorbike.

  val initialSpeed = s

  val game = new TheChasm.Game(initialSpeed, gapStart, landingStart, landingEnd)
  val start = Running(game, initialSpeed)
  for (action <- game.findSuccessfulActionSequence(start)) {
    val command = action match {
      case Speed => "SPEED"
      case Slow => "SLOW"
      case Wait => "WAIT"
      case Jump => "JUMP"
    }
    println(command)
  }
}

object TheChasm {

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

    val onRunway = posOnRunway(pos)
    val onLanding = posOnLanding(pos)

    def posOnRunway(pos: Int) =
      0 <= pos && pos < game.gapStart

    def posOnLanding(pos: Int) =
      game.landingStart <= pos && pos < game.landingEnd

    override def next(action: Action): State = {
      if (speed == 0 && action != Speed) {
        throw new IllegalArgumentException("Must Speed when speed is 0")
      }
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

    def findSuccessfulActionSequence(start: State = initialState): List[Action] = {
      def findSolution(state: State, acc: List[Action] = Nil): (Boolean, List[Action]) = {
        state match {
          case Dead => (false, Nil)
          case Landed => (true, acc)
          case x: Running =>
            val possibleActions = getPreferredActions(x)
            var index = 0
            while (index < possibleActions.size) {
              val action = possibleActions(index)
              val newState = state.next(action)
              val (success, sequence) = findSolution(newState, action :: acc)
              if (success) return (success, sequence)
              index = index + 1
            }
            (false, Nil)
        }
      }
      findSolution(start)._2.reverse
    }

    def jumpWouldHelp(state: State) = {
      state.next(Jump) match {
        case Dead => false
        case x: Running => x.onLanding
      }
    }

    def getPreferredActions(state: Running): List[Action] = {
      if (state.onRunway) {
        if (state.speed == 0) List(Speed)
        else if (jumpWouldHelp(state)) List(Jump)
        else List(Speed, Wait, Slow)
      }
      else List(Slow)
    }
  }
}
