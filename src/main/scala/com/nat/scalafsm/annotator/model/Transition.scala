package com.nat.scalafsm.annotator.model


case class Transition[A](
  name: String,
  to: String,
  conditions: List[String],
  effects: List[String]
)