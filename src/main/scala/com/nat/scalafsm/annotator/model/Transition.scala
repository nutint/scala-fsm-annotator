package com.nat.scalafsm.annotator.model


case class Transition[A<:State[A]](
  name: String,
  to: A,
  guardCondition: String,
  sideEffect: String
)