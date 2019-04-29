package com.nat.scalafsm.annotator.processor

import com.nat.scalafsm.annotator.model.{ Diagram => MDiagram, State => MState, Transition => MTransition }
import com.nat.scalafsm.annotator.api.{ State, Transition, Diagram }
import scala.reflect.runtime.{universe => u}

class AnnotationProcessor {
  import u._
  import AnnotationProcessor._

  def parse[A](staticClass: String): Either[String,MDiagram[A]] = {
    parsedDiagram[A](staticClass)
      .map(_.resolve)
      .getOrElse(Left(s"Specified class $staticClass is not annotated with Diagram"))
  }

  def isDiagramAnnotation(a: u.Annotation): Boolean = {
    a match {
      case s: com.nat.scalafsm.annotator.api.Diagram => {
        println("is diagram")
        true
      }
      case _ => {
        println("is not diagram")
        false
      }
    }
  }

  def toDiagram(a: u.Annotation): Diagram = {
    a.asInstanceOf[com.nat.scalafsm.annotator.api.Diagram]
  }

  def parse2[SDiagram](staticClass: String)(implicit sttag: TypeTag[SDiagram]): Unit = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val stateAnnotation: Option[SDiagram] = annotatedClass.annotations.collectFirst { case s: SDiagram => s }
    println(s"stateAnnotation = $stateAnnotation")
//    val methodAnnotations: List[TransitionAnnotation] = annotatedClass.info.decls.flatMap(_.annotations.collect { case t: TransitionAnnotation => t }).toList
//    val subclasses: Set[ClassSymbol] = annotatedClass.knownDirectSubclasses.collect { case sc: ClassSymbol => sc }
  }

  def findAnnotation[A, DAnnotation](staticClass: String)(implicit sttag: TypeTag[DAnnotation]): Option[DAnnotation] = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    annotatedClass.annotations.collectFirst{ case s: DAnnotation => s }
  }

  def findAnnotation2[A](staticClass: String)(implicit sttag: TypeTag[Diagram], stag: TypeTag[String]): Option[Diagram] = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)

    val mirror = runtimeMirror(cls.getClassLoader)
    val clsSymbol = mirror.staticClass(cls.getCanonicalName)
    val annotations = clsSymbol.annotations

    val res = ListBuffer[T]()
    annotatedClass.annotations.find{_.tree.tpe =:= u.typeOf[Diagram]}
      .map { a =>
        println("tail = " + a.tree.children.tail)
//        a.tree.children.tail.head match {
//          case Literal(Constant(name: String)) => Diagram(name, Nil)
//          case _ => Diagram("aaa", Nil)
//        }
//
//        a.tree.children.tail.tail.head match {
//          case Literal(Constant(ts: List[Transition @unchecked])) => Diagram("xx", ts)
//          case _ => Diagram("aaa", Nil)
//        }
//        val children = a.tree.children
//        Diagram(children.head, children.tail.head)

        val anntCls = a.tree.tpe.typeSymbol.asClass
        val classMirror = mirror.reflectClass(anntCls)
        val anntType = annt.tree.tpe
        val constructor = anntType.decl(termNames.CONSTRUCTOR).asMethod
        val constructorMirror = classMirror.reflectConstructor(constructor)
        val instance = a.tree match {
          case Apply(c, args: List[Tree]) =>
            val res = args.collect( {
              case i: Tree =>
                i match {
                  case Literal(Constant(value)) =>
                    value
                }
            })
            constrctor
        }
      }
  }

  def parsedDiagram[A](staticClass: String)(implicit sttag: TypeTag[Diagram]): Option[UnresolvedDiagram[A]] = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    println(s"annotatedClass = ${annotatedClass.annotations}")
//    println(s"toDiagram = ${toDiagram(annotatedClass.annotations.head)}")
//    val diagramAnnotation: Option[Diagram] = annotatedClass.annotations.collectFirst{ case s: Diagram => s }
    val diagramAnnotation: Option[Diagram] = findAnnotation2[A](staticClass)
    println(s"diagramAnnotation = $diagramAnnotation")
    diagramAnnotation.map { diagram =>
      val subclasses = annotatedClass.knownDirectSubclasses.collect({ case sc: ClassSymbol => sc })
      UnresolvedDiagram[A](
        diagram.name,
        diagram.initialTransitions.map { it =>
          UnresolvedTransition[A](
            it.eventName,
            InitialStateId,
            InternalStateId(it.nextState),
            it.guards,
            it.effects)
        },
        subclasses.flatMap(parseState[A]).toList
      )
    }
  }

  def parseState[A](classSymbol: ClassSymbol): Option[UnresolvedState[A]] = {
    val stateAnnotation = classSymbol.annotations.collectFirst({ case s: State => s})
    val transitionAnnotations: List[Transition] = classSymbol.info.decls.flatMap(_.annotations.collect{ case t: Transition => t }).toList
    stateAnnotation.map { foundStateAnnotation =>
      UnresolvedState[A](
        foundStateAnnotation.id,
        foundStateAnnotation.name,
        transitionAnnotations.map { ta =>
          UnresolvedTransition[A](
            ta.eventName,
            InternalStateId(foundStateAnnotation.id),
            InternalStateId(ta.nextState),
            ta.guards,
            ta.effects
          )
        }
      )
    }
  }
}

object AnnotationProcessor {
  case class UnresolvedTransition[A](name: String, from: StateId, to: StateId, guardCondition: List[String], sideEffects: List[String]) {
    def resolve(states: List[UnresolvedState[A]]): Either[String,MTransition[A]] =
      states
        .find(state => to.equal(state.id))
        .map { targetState =>
          Right(MTransition[A](
            name,
            targetState.id,
            guardCondition,
            sideEffects
          ))
        }
        .getOrElse(Left(s"Resolving transition failed: Unable to resolve next state id $to"))
  }

  case class UnresolvedState[A](id: String, name: String, unresolvedTransitions: List[UnresolvedTransition[A]]) {
    def resolve(states: List[UnresolvedState[A]]): Either[String, MState[A]] = {
      unresolvedTransitions
        .map(_.resolve(states))
        .foldLeft((List[String](), List[MTransition[A]]())) { (acc, elem) =>
          elem match {
            case Left(msg) => acc.copy(_1 = acc._1 :+ msg)
            case Right(ts) => acc.copy(_2 = acc._2 :+ ts)
          }
        } match {
        case (Nil, tss) => Right(MState[A](id, name, tss))
        case (errs, _) => Left(s"Resolving state failed: ($id, $name, $errs)")
      }
    }
  }

  case class UnresolvedDiagram[A](name: String, initialTransitions: List[UnresolvedTransition[A]], states: List[UnresolvedState[A]]) {
    def resolve: Either[String, MDiagram[A]] = {
      val resolvedStateResult = states
        .map(_.resolve(states))
        .foldLeft((
          List[String](),
          List[MState[A]]()
        )) { (acc, elem) => {
          elem match {
            case Left(msg) => acc.copy(_1 = acc._1 :+ msg)
            case Right(ts) => acc.copy(_2 = acc._2 :+ ts)
          }
        }
      } match {
        case (Nil, ss) => Right(ss)
        case (errs, _) => Left(s"Resolving State failed: $errs")
      }

      val resolvedInitialTransitionResult =
        initialTransitions
          .map(_.resolve(states))
          .foldLeft((
            List[String](),
            List[MTransition[A]]()
          )) { (acc, elem) => {
              elem match {
                case Left(msg) => acc.copy(_1 = acc._1 :+ msg)
                case Right(ts) => acc.copy(_2 = acc._2 :+ ts)
              }
            }
          } match {
          case (Nil, ss) => Right(ss)
          case (errs, _) => Left(s"Resolving Transitions failed: $errs")
        }

      (resolvedStateResult, resolvedInitialTransitionResult) match {
        case (Left(sErrs), Left(tErrs)) => Left(s"Resolving diagram($name) failed with $sErrs, and $tErrs")
        case (Left(sErrs), _ ) => Left(s"Resolving diagram($name) failed with $sErrs")
        case (_, Left(tErrs) ) => Left(s"Resolving diagram($name) failed with $tErrs")
        case (Right(s), Right(t)) => Right(MDiagram[A](name, t, s))
      }
    }
  }

  trait StateId {
    def equal(id: String): Boolean
  }
  case object InitialStateId extends StateId {
    override def equal(id: String): Boolean = false
  }
  case class InternalStateId(id: String) extends StateId {
    override def equal(rhs: String): Boolean = rhs == id
  }
}