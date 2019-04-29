package com.nat.scalafsm.annotator

import java.io.File

import model._
import net.sourceforge.plantuml.SourceStringReader

// Usage
//trait PersonState extends State[PersonState]
//
//case object PersonSitState extends PersonState {
//  override val name: String = "Sit"
//  override val id: String = "sit"
//  override val transitions: List[Transition[PersonState]] = List(
//    Transition("Stand up", PersonStandState, "", ""),
//    Transition("Lie down", PersonLaidState, "", "")
//  )
//}
//
//case object PersonStandState extends PersonState {
//  override val name: String = "Stand"
//  override val id: String = "stand"
//  override val transitions: List[Transition[PersonState]] = List(
//    Transition("Got kicked", PersonLaidState, "", "Crying out loud"),
//    Transition("Sit down", PersonSitState, "", "")
//  )
//}
//
//case object PersonLaidState extends PersonState {
//  override val name: String = "Laid down"
//  override val id: String = "laiddown"
//  override val transitions = List(
//    Transition("Get up", PersonSitState, "", "")
//  )
//}
//
//class Parser[A<:State[A]](diagram: Diagram[A]) {
//  def parse: String = {
//    s"""
//       |@startuml
//       |title ${diagram.name}
//       |
//       |$parseInitialStates
//       |$parseNonInitialStates
//       |@enduml
//     """.stripMargin
//  }
//
//  def parseNonInitialStates =
//    diagram
//      .states
//      .map(s => parseState(s))
//      .mkString("\n")
//
//  def parseInitialStates: String = {
//    diagram.initialStates
//      .map(s => s"[*] --> ${s.id}")
//      .mkString("\n")
//  }
//
//  def parseState(state: State[A]) =
//    state
//      .transitions
//      .map(t => s"${state.id} --> ${t.to.id} : ${t.name}")
//      .mkString("\n")
//}
//
///**
//  * Experiment Checklist
//  * [.] State and Transition with type control
//  *   - [X] Compiled with scala type system
//  *   - [.] Test with incompatible types
//  *   - [X] Processing
//  */
//object Sandbox {
//  println("running sandbox")
//
//  PersonSitState
//    .processEvent("Stand up")
//    .flatMap(_.processEvent("Sit down"))
//    .flatMap(_.processEvent("Stand up"))
//    .flatMap(_.processEvent("Got kicked"))
//
//  val diagram = Diagram[PersonState](
//    "Person Pose Diagram",
//    List(PersonSitState),
//    List(PersonSitState, PersonStandState, PersonLaidState)
//  )
//
//  val parser = new Parser[PersonState](diagram)
//  val parsedString = parser.parse
//  println(s"parsedString = \n$parsedString")
//
//
//  val reader: SourceStringReader = new SourceStringReader(parsedString)
//  reader.generateImage(new File("/Users/nat/Documents/bobalice.png"))
//
//}
