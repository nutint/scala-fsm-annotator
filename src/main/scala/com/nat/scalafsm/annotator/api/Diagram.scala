package com.nat.scalafsm.annotator.api

import scala.annotation.StaticAnnotation
//import scala.reflect.runtime.universe.{ Annotation => StaticAnnotation }

case class Diagram(name: String, initialTransitions: List[Transition]) extends StaticAnnotation
