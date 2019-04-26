package com.nat.scalafsm.annotator.api

import scala.annotation.StaticAnnotation

case class State(name: String) extends StaticAnnotation
