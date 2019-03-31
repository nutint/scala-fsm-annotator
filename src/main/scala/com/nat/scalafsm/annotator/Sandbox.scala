package com.nat.scalafsm.annotator

import scala.util.Try


// Libraries
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

case class Transition[A<:State[A]](
  name: String,
  to: A,
  guardCondition: String,
  sideEffect: String
)

// Usage
trait PersonState extends State[PersonState]

case object PersonSitState extends PersonState {
  override val name: String = "Sit"
  override val transitions: List[Transition[PersonState]] = List(
    Transition("Stand up", PersonStandState, "", ""),
    Transition("Lie down", PersonLaidState, "", "")
  )
}

case object PersonStandState extends PersonState {
  override val name: String = "Stand"
  override val transitions: List[Transition[PersonState]] = List(
    Transition("Got kicked", PersonLaidState, "", "Crying out loud"),
    Transition("Sit down", PersonSitState, "", "")
  )
}

case object PersonLaidState extends PersonState {
  override val name: String = "Laid down"
  override val transitions = List(
    Transition("Get up", PersonSitState, "", "")
  )
}

/**
  * Experiment Checklist
  * [.] State and Transition with type control
  *   - [X] Compiled with scala type system
  *   - [.] Test with incompatible types
  *   - [X] Processing
  */
object Sandbox {
  println("running sandbox")

  PersonSitState
    .processEvent("Stand up")
    .flatMap(_.processEvent("Sit down"))
    .flatMap(_.processEvent("Stand up"))
    .flatMap(_.processEvent("Got kicked"))
}
