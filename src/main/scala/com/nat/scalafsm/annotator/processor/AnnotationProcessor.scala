package com.nat.scalafsm.annotator.processor

import com.nat.scalafsm.annotator.model.{ Diagram => MDiagram, State => MState, Transition => MTransition }
import com.nat.scalafsm.annotator.api.{ State, Transition, Diagram }
import scala.reflect.runtime.{universe => u}

class AnnotationProcessor {
  import u._

  def parse[A](staticClass: String): MDiagram[A] = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val diagramAnnotation: Option[Diagram] = annotatedClass.annotations.collectFirst{ case s: Diagram => s }
    diagramAnnotation.map { diagram =>
      val subclasses = annotatedClass.knownDirectSubclasses.collect({ case sc: ClassSymbol => sc })
      subclasses
    }
    MDiagram[A]("name", Nil, Nil)
  }

  def parseState[A](classSymbol: ClassSymbol): Option[MState[A]] = {
    val stateAnnotation = classSymbol.annotations.collectFirst({ case s: State => s})
    val transitions = parseTransition[A](classSymbol)

    stateAnnotation.map { stateAnno =>
      new MState[A] {
        override def name: String = stateAnno.name

        override def id: String = stateAnno.id

        override def transitions: List[MTransition[A]] = transitions.map { transition =>
          MTransition[A](
            transition.name,
          )


//          name: String,
//          to: State[A],
//          guardCondition: String,
//          sideEffect: String
        }
      }
    }
  }

  def parseTransition[A](classSymbol: ClassSymbol): List[Transition] = {
    classSymbol.info.decls.flatMap(_.annotations.collect{ case t: Transition => t }).toList
  }

  def parse[StateAnnotation, TransitionAnnotation](staticClass: String)(implicit sttag: TypeTag[StateAnnotation], tttag: TypeTag[TransitionAnnotation]): Unit = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val stateAnnotation: Option[Diagram] = annotatedClass.annotations.collectFirst{ case s: Diagram => s }
    val methodAnnotations: List[TransitionAnnotation] = annotatedClass.info.decls.flatMap(_.annotations.collect{ case t: TransitionAnnotation => t }).toList
    val subclasses: Set[ClassSymbol] = annotatedClass.knownDirectSubclasses.collect{ case sc: ClassSymbol => sc }

    println(s"Subclasses = $subclasses\n")

//    subclasses.toList
//      .map(sc => (parseState(sc), parseTransition(sc)))
//      .foreach(println)

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
}
