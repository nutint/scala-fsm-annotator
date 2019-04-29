package com.nat.scalafsm.annotator.model

case class Diagram[A](
  name: String,
  initialTransitions: List[Transition[A]],
  states: List[State[A]]
)
