package com.typeclassified.hmm.cssr.parse

object Alphabet {
  def apply(alphabetRaw: String) = new Alphabet(alphabetRaw)
}

class Alphabet(alphabetRaw: String) {
  val raw = alphabetRaw.filterNot("\r\n".contains(_)).split()
  val map: Map[Char, Int]= raw.zipWithIndex.map { case (c: Char, i: Int) => c -> i }.toMap
  val length = raw.length

  override def toString: String = s"[${raw.mkString(", ")}]"
}

object AlphabetHolder {
  var alphabet: Alphabet = _
}


