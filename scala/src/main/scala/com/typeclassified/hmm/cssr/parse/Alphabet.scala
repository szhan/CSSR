package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.Aliases.Event

object Alphabet {
  def apply(alphabetRaw: String, delim:String) = new Alphabet(alphabetRaw, delim)
}

class Alphabet(alphabetRaw: String, val delim:String) {
  val raw:Array[Event] = alphabetRaw.filterNot("\r\n".contains(_)).split(delim)
  val map: Map[Event, Int]= raw.zipWithIndex.map { case (s: Event, i: Int) => s -> i }.toMap
  val length = raw.length

  override def toString: String = s"[${raw.mkString(delim + " ")}]"
}

object AlphabetHolder {
  var alphabet: Alphabet = _
}


