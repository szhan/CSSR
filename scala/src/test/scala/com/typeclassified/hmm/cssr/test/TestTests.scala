package com.typeclassified.hmm.cssr.test

import com.typeclassified.hmm.cssr.parse.{AlphabetHolder, Alphabet}
import com.typeclassified.hmm.cssr.trees.ParseTree
import org.scalatest.{FlatSpec, Matchers}

class TestTests extends FlatSpec with Matchers {
  AlphabetHolder.alphabet = new Alphabet("abc", "")
  var alphabet = AlphabetHolder.alphabet
  var tree = new ParseTree(alphabet)

  behavior of "Test.move"

  it should "move a Leaf from one EquivalenceClass to another" in {
    pending
  }
}
