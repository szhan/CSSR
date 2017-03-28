package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.Aliases.Event

object Alphabet {
  def apply(alphabetRaw: String, delim:String) = new Alphabet(alphabetRaw, delim)
  def delim(str:String, delim:String):Array[Event] = {
    if (delim.length > 1) {
      throw new Exception("delimiter must be single character")
    } else if (delim == "") {
      str.filterNot("\r\n".contains(_)).split(delim).filterNot(_ == "")
    } else {
      str.filterNot("\r\n".contains(_)).split(delim.head).filterNot(_ == "")
    }
  }
}

class Alphabet(alphabetRaw: String, val delim:String) {

  val raw:Array[Event] = Alphabet.delim(alphabetRaw, delim)
  val map: Map[Event, Int]= raw.zipWithIndex.map { case (s: Event, i: Int) => s -> i }.toMap
  val length = raw.length

  override def toString: String = s"[${raw.mkString(delim + " ")}]"
}

object AlphabetHolder {
  var alphabet: Alphabet = _
}


