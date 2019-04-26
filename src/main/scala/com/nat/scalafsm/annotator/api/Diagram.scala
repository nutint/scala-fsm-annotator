package com.nat.scalafsm.annotator.api

import scala.annotation.StaticAnnotation

case class Diagram(name: String, initialTransitions: List[Transition]) extends StaticAnnotation
