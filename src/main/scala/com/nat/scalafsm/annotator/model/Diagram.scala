package com.nat.scalafsm.annotator.model

case class Diagram[A<:State[A]](
  name: String,
  states: List[State[A]]
)
