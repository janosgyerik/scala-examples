package com.janosgyerik.scala.practice

object SafeCracker {
  val digits = 2

  def cracker(): String = cracker("1234")

  def cracker(seq: String) = {
    seq
    // while set.size of unique combinations is < 10^digits
    // find a shortest suitable sequence to append
    // for toAppendLength in (1 to digits)
    //  iter = perm(toAppendLength)
    //  prefix = seq.takeRight(digits - toAppendLength)
    //  while iter.hasNext
    //    next = iter.next
    //    candidate = prefix + next
    //    try if this is a new combination: candidate
    //      if yes: return f(unique + candidate, seq + next)
  }

  def combinations(prefix: String) = {
    prefix.toList.map(x => x - '0')
  }

  def perm(n: Int): List[List[Int]] = n match {
    case 0 => List()
    case 1 => (0 to 9).map(x => List(x)).toList
    case _ => (0 to 9).map(x => perm(n - 1).map(y => List(x) ++ y)).toList.flatten
  }
}
