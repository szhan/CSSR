package com.typeclassified.hmm.cssr.state

import breeze.linalg.{DenseVector, sum}
import com.typeclassified.hmm.cssr.Aliases.Event
import com.typeclassified.hmm.cssr.CSSR.{StateToStateTransitions, TransitionState}

class AllStates (eqClasses:Iterable[State], val transitionMap:StateToStateTransitions) {
  val states      = eqClasses.toArray
  val transitions:Array[Map[Event, TransitionState]] = states.map{ state => transitionMap(state) }

  val stateIndexes:Array[Set[Int]]        = states.map{_.histories.flatMap{_.locations.keySet}.toSet}
  val stateMap:Map[State, Int] = states.zipWithIndex.toMap

  val frequency:DenseVector[Double]    = new DenseVector[Double](stateIndexes.map{_.size.toDouble})
  val distribution:DenseVector[Double] = frequency :/ sum(frequency)
}
