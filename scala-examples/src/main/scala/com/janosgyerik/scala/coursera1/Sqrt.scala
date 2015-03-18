package com.janosgyerik.scala.coursera1

object Sqrt {
  def sqrt(x: Double): Double = {
    require(x > 0, "Argument must be positive")
    sqrt(x, 1e-15)
  }

  def sqrt(x: Double, error: Double): Double = {
    def closeEnough(q: Double) = {
      Math.abs(x / q - q) < error
    }

    def nextGuess(q: Double): Double = {
      (q + x / q) / 2
    }

    def approximate(q: Double): Double = {
      if (closeEnough(q)) q else approximate(nextGuess(q))
    }

    approximate(2)
  }
}
