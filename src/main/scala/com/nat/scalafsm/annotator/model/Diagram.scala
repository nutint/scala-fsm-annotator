package com.nat.scalafsm.annotator.model

case class Diagram[A<:State[A]](
  name: String,
  initialStates: List[State[A]],
  states: List[State[A]]
)
