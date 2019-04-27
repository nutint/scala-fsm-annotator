package com.nat.scalafsm.annotator.model

case class Diagram[A<:State[A]](
  name: String,
  initialTransitions: List[Transition[A]],
  states: List[State[A]]
)
