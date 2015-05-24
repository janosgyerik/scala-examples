package com.janosgyerik.scala.practice

object SafeCracker {

  def genCrackerString(symbols: String, codeLength: Int) = {
    require(symbols.nonEmpty)
    require(codeLength > 0)

    val combinations = math.pow(symbols.length, codeLength)

    def cracker(prefix: String, used: Set[String]): String = {

      def findSuffixCombo(num: Int): (String, String) = {
        val suffix = getNth(symbols, num)
        val combo = (prefix + suffix).takeRight(codeLength)

        if (!used.contains(combo)) (suffix, combo)
        else findSuffixCombo(num + 1)
      }

      if (used.size == combinations) prefix
      else {
        val (suffix, combo) = findSuffixCombo(0)
        cracker(prefix + suffix, used + combo)
      }
    }

    val first = symbols(0).toString * codeLength

    cracker(first, Set(first))
  }

  def getNth(symbols: String, num: Int) = {
    val base = symbols.length

    def getNth(prefix: String, num: Int): String = {
      if (num == 0) {
        if (prefix.isEmpty) symbols(0).toString
        else prefix
      } else {
        val index = num % base
        val digit = symbols(index)
        getNth(digit + prefix, num / base)
      }
    }
    getNth("", num)
  }
}
