package com.nat.scalafsm.annotator.model


trait State[A] {
  def name: String
  def transitions: List[Transition[State[A]]]
}