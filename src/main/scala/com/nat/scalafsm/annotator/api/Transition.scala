package com.nat.scalafsm.annotator.api

import scala.annotation.StaticAnnotation

case class Transition(eventName: String, nextState: String, guards: List[String] = Nil, effects: List[String] = Nil) extends StaticAnnotation
