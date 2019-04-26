package com.nat.scalafsm.annotator.api

import scala.annotation.StaticAnnotation

case class State(id: String, name: String) extends StaticAnnotation
