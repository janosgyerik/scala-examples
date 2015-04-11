package com.janosgyerik.scala.codility

object FindFirstMissingInt {

  def findFirstMissingInt(ints: Array[Int]) = {
    val unique = ints.toSet
    (1 to ints.length + 1).dropWhile(unique.contains).head
  }

}
