package com.nat.scalafsm.annotator

import model._

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
