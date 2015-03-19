package com.janosgyerik.scala.practice

object SafeCracker {
  val digits = 2

  def cracker(): String = cracker("1234")

  def cracker(seq: String) = {
    seq
  }

  def combinations(prefix: String) = {
  }

  def perm(n: Int): List[List[Int]] = n match {
    case 0 => List()
    case 1 => (0 to 9).map(x => List(x)).toList
    case _ => (0 to 9).map(x => perm(n - 1).map(y => List(x) ++ y)).toList.flatten
  }
}
