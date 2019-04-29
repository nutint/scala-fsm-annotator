package com.nat.scalafsm.annotator

import shapeless.Typeable

import scala.annotation.StaticAnnotation
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => u}
import scala.util.{Failure, Success, Try => STry}

case class State2[A](name: String, desc: String, data: A, enabled: Boolean) extends StaticAnnotation

case class MethodAnnotation[A](name: String) extends StaticAnnotation

@State2[Int]("aa", "description",  1,true)
@State2[String]("aa", "description",  "str",true)
case class TestClass(x: String) {
  @MethodAnnotation[Int]("foo")
  def foo(param: String) = param
}

case class SimpleStateDiagram[A](name: String, initialStates: List[SimpleState[A]], states: List[SimpleState[A]]) extends StaticAnnotation
case class SimpleState[A](name: String) extends StaticAnnotation
case class SimpleTransition[A](name: String) extends StaticAnnotation


case class SimpleStateDiagram2(name: String, initialStates: List[SimpleState2], states: List[SimpleState2]) extends StaticAnnotation
case class SimpleState2(name: String) extends StaticAnnotation
case class SimpleTransition2(name: String) extends StaticAnnotation

@SimpleStateDiagram2(
  "Washing Machine State2",
  List(
    SimpleState2("off")
  ),
  List(
    SimpleState2("off"),
    SimpleState2("on")
  )
)
sealed trait WashingMachineState2
@SimpleState2("off")
case class WashingMachineState2Off() extends WashingMachineState2 {
  @SimpleTransition("Power On")
  def powerOn = WashingMachineState2On
}
@SimpleState2("on")
case class WashingMachineState2On() extends WashingMachineState2 {
  @SimpleTransition2("Power Off")
  def powerOff = WashingMachineState2Off()
}


@SimpleStateDiagram[FridgeState](
  "Fridge state",
  Nil,
  Nil
)
sealed trait FridgeState
case class FridgeStateOff() extends FridgeState {
  @SimpleTransition[FridgeState]("Power on")
  def powerOn = FridgeStateOn()
}
case class FridgeStateOn() extends FridgeState {
  @SimpleTransition[FridgeState]("Power off")
  def powerOff = FridgeStateOff()
}

class AParser {
  import u._
  import shapeless.syntax.typeable._

  def parse3[A](staticClass: String)(implicit tt: Typeable[A], st: Typeable[SimpleStateDiagram[A]]) = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val diagramAnnotationnn: Option[StaticAnnotation] = annotatedClass.annotations.head.cast[StaticAnnotation]
    println(s"diagram.head = ${annotatedClass.annotations.head}")
    println(s"diagramAnnotationnn = $diagramAnnotationnn")
    val diagramAnnotation: Option[StaticAnnotation] =
      annotatedClass.annotations
        .collectFirst{
          case s if s.isInstanceOf[SimpleStateDiagram[A]] => s.asInstanceOf[SimpleStateDiagram[A]]
          case s: SimpleStateDiagram[A] => s
          case s if s.cast[SimpleStateDiagram[A]].nonEmpty => s.cast[SimpleStateDiagram[A]].get
          case s: StaticAnnotation => s
          case s if s.cast[StaticAnnotation].nonEmpty => s.cast[StaticAnnotation].get
        }
    println(s"diagramAnnotation = $diagramAnnotation")
  }

  def toSimpleStateDiagram[A](an: Annotation)(implicit tt: Typeable[A]): Option[SimpleStateDiagram[A]] = {
    val res = an.cast[SimpleStateDiagram[A]]
    println(s"res = $res")
    res
  }

  def toSimple[A](san: StaticAnnotation): Option[SimpleStateDiagram[A]] = {
    san.cast[SimpleStateDiagram[A]]
  }

  def parse[StateAnnotation, TransitionAnnotation](staticClass: String)(implicit sttag: TypeTag[StateAnnotation], tttag: TypeTag[TransitionAnnotation]): Unit = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val stateAnnotation: Option[StateAnnotation] = annotatedClass.annotations.collectFirst{ case s: StateAnnotation => s }
    val methodAnnotations: List[TransitionAnnotation] = annotatedClass.info.decls.flatMap(_.annotations.collect{ case t: TransitionAnnotation => t }).toList
    val subclasses: Set[ClassSymbol] = annotatedClass.knownDirectSubclasses.collect{ case sc: ClassSymbol => sc }

    println(s"Subclasses = $subclasses\n")

    subclasses.toList
        .map(sc => (parseState(sc), parseTransition(sc)))
        .foreach(println)

    println()

    println(s"stateAnnotation = $stateAnnotation, and transitionAnnotation = $methodAnnotations")
    println(s"stateAnnotation.getClass = ${stateAnnotation.getClass}, and methods.getClass = ${methodAnnotations.getClass}")
    println()
    stateAnnotation
      .map {
        // case s: StaticAnnotation => println("Found static annotation")
        case s: StateAnnotation => println("state2")
        case _ => println("not found any annotation")
      }

    methodAnnotations
      .map {
        case m: TransitionAnnotation => println("found Transition")
        case _ => println("not found transition annotation")
      }
  }

  def parseTransition[TransitionAnnotation](clazz: ClassSymbol): List[TransitionAnnotation] = {
    clazz.info.decls
      .filter(_.isMethod)
      .toList
      .flatMap(_.annotations.collect { case ma: TransitionAnnotation => ma })
  }

  def parseState[StateAnnotation](clazz: ClassSymbol): Option[StateAnnotation] = {
    clazz.annotations
      .collectFirst{ case sa: StateAnnotation => sa }
  }

  def mapDebug[A](va: A):A = {
    println(s"mapDebug $va")
    va
  }

  def simpleParse[A](staticClass: String)(implicit sttag: TypeTag[SimpleStateDiagram[A]]): STry[SimpleStateDiagram[A]] = {
    getClassSymbol(staticClass)
      .map[Option[SimpleStateDiagram[A]]](_.annotations.collectFirst{ case s: SimpleStateDiagram[A] => s })
      .flatMap {
        case Some(ann) => Success(ann)
        case None =>  Failure(new IllegalArgumentException(s"Provided class is not annotated to SimpleStateDiagram"))
      }
  }

  def getClassSymbol(staticClass: String): STry[ClassSymbol] = STry {
    runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
  }

  def isExpectedAnnotation[EXA](annotation: Annotation)(implicit ttag: TypeTag[EXA]): Boolean = {
    annotation.tree.tpe =:= typeOf[EXA]
  }

  def findingDirectSubclasses(typ: Type): List[ClassSymbol] = {
    typ.typeSymbol.asClass.knownDirectSubclasses
      .map(_.asInstanceOf[ClassSymbol])
      .toList
  }
}


object Sandbox2 {
  println("Running sandbox 2")

  val myAnnotatedClass: u.ClassSymbol = u.runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass("com.nat.scalafsm.annotator.TestClass")
  val annotation = myAnnotatedClass.annotations.find(_.tree.tpe =:= u.typeOf[State2[String]])
  val res = annotation.map(_.tree.children.tail match {
    case name :: desc :: data :: enabled :: _ => (name, desc, data, enabled)
  })

  val res2 = annotation.flatMap { a =>
    a.tree.children.tail.collectFirst {
      case u.Literal(u.Constant(name: String)) => name
    }
  }
  println(s"annotation = $res, and $res2")


  // Finding method annotation
  val testClass = TestClass("testParam")
  val mirror = u.runtimeMirror(testClass.getClass.getClassLoader)
  val instantMirror = mirror.reflect(testClass)
  val testClassSymbol = mirror.classSymbol(testClass.getClass)

  val annotations2 = testClassSymbol.info.decls
    .filter(_.isMethod)
      .flatMap(_.annotations)

  println(s"suspect annotation = $annotations2")

  val primeConstructor = testClassSymbol.info.decls.filter(m => m.isMethod && m.asMethod.isPrimaryConstructor).head
  println(s"primeCOnstructor = $primeConstructor")
  val params = primeConstructor.typeSignature.paramLists.head
  println(s"params = $params")

  val parser = new AParser
  parser.parse[State2[Int], MethodAnnotation[Int]]("com.nat.scalafsm.annotator.TestClass")
  parser.getClassSymbol("com.nat.scalafsm.annotator.TestClass")
    .map(_.toString)
    .foreach(c => println(s"got class symbol = $c"))

  val tpe: u.Type = u.typeOf[WashingMachineState2]



  // Simple Parse
  val simRes = parser.simpleParse[WashingMachineState2]("com.nat.scalafsm.annotator.WashingMachineState2")
  println(s"simRes = $simRes")
}

object Sandbox3U extends App {
  import u._
  val parser = new AParser()
//  parser.parse[SimpleStateDiagram2, SimpleTransition2]("com.nat.scalafsm.annotator.WashingMachineState2")



  parser.parse3[FridgeState]("com.nat.scalafsm.annotator.FridgeState")

  import shapeless.syntax.typeable._
  def checkType[A](any: Any)(implicit tt: Typeable[A]): List[A] = {
    any.cast[List[A]].get
  }

  println(checkType[String](List("123")))
  println(checkType[SimpleStateDiagram[FridgeState]](List(SimpleStateDiagram[FridgeState]("xx", Nil, Nil))))

  val fridgeClass = parser.getClassSymbol("com.nat.scalafsm.annotator.FridgeState").map(_.annotations)
      .map{ anns =>
        println(s"anns = $anns")
        anns.map { ann =>
          parser.toSimpleStateDiagram[FridgeState](ann)
        }
        .foreach(println)
      }

  val an: StaticAnnotation = SimpleStateDiagram[FridgeState]("xx", Nil, Nil)

  def toSimple[A](san: StaticAnnotation): Option[SimpleStateDiagram[A]] = {
    san.cast[SimpleStateDiagram[A]]
  }

  println(toSimple[FridgeState](an))

}