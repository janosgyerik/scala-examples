package com.janosgyerik.scala.scratch

object Pascal {
  def pascal(c: Int, r: Int): Int = {
    require(!(c < 0 || r < 0 || c > r), "c, r must be both positive and c <= r")
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def main(args: Array[String]) {
    println(pascal(3, 4))
    println(pascal(3, 3))
    //println(pascal(4, 3))
  }
}
