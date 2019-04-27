package com.nat.scalafsm.annotator.model

import scala.util.Try


case class State[A<:State[A]](id: String, name: String, transitions: List[Transition[A]]) {
  def processEvent(event: String): Try[String] = Try {
    transitions
      .find(_.name == event)
      .map(executeTransition)
      .map(_.to)
      .getOrElse(throw new IllegalArgumentException(s"Unable to process event $event, for current state $name"))
  }

  def executeTransition(transition: Transition[A]): Transition[A] = {
    println(s"executing transition ${transition.name}, toNextState ${transition.to}, andDoingSideEffect ${transition.effects}")
    transition
  }
}