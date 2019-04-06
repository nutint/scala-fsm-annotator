package com.nat.scalafsm.annotator.model

import scala.util.Try


trait State[A<:State[A]] {
  def name: String
  def transitions: List[Transition[A]]

  def processEvent(event: String): Try[A] = Try {
    transitions
      .find(_.name == event)
      .map(executeTransition)
      .map(_.to)
      .getOrElse(throw new IllegalArgumentException(s"Unable to process event $event, for current state $name"))
  }

  def executeTransition(transition: Transition[A]): Transition[A] = {
    println(s"executing transition ${transition.name}, toNextState ${transition.to.name}, andDoingSideEffect ${transition.sideEffect}")
    transition
  }
}