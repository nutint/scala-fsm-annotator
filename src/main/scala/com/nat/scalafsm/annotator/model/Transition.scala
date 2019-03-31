package com.nat.scalafsm.annotator.model

case class Transition[A](
  name: String,
  fn: State[A] => State[A],
  guardCondition: String,
  sideEffect: String
)