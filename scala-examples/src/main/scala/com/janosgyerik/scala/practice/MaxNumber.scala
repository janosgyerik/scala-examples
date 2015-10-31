package com.janosgyerik.scala.practice

object MaxNumber {
  def solve(nums: Seq[Int]): Long = {
    nums.map(_.toString).sortWith(
      (a, b) => (a concat b).compareTo(b concat a) > 0).mkString.toLong
  }
}
