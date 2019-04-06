package com.nat.scalafsm.annotator.model


case class Transition[A<:State[A]](
  name: String,
  to: State[A],
  guardCondition: String,
  sideEffect: String
)