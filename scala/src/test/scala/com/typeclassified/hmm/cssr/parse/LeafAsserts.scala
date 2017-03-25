package com.typeclassified.hmm.cssr.parse

import com.typeclassified.hmm.cssr.Aliases.Event
import com.typeclassified.hmm.cssr.trees.ParseLeaf
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer

trait LeafAsserts extends Matchers {
  def asEvents(cs:String):List[Event] = cs.split("").toList
  def asEvents(as:String, bs:String*):List[List[String]] = as.split("").toList +: bs.map(_.split("").toList).toList

  def assertLeafProperties(leaf:ParseLeaf, obs:List[String]): Unit = {
    leaf.observed    should equal (obs)
    leaf.observation should equal (obs.head)
  }

  def assertChildrenByExactBatch(pleaf:ParseLeaf, expected:List[List[Event]]):Unit = assertChildrenByExactBatch(pleaf.children, expected)

  def assertChildrenByExactBatch(children:ListBuffer[ParseLeaf], expected:List[List[Event]]):Unit = {
    children                    should have    size              expected.toList.length
    children.map(_.observed)    should contain theSameElementsAs expected
    children.map(_.observation) should contain theSameElementsAs expected.map(_.head)
  }
}
