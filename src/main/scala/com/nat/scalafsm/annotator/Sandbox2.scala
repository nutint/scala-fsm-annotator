package com.nat.scalafsm.annotator

import scala.annotation.StaticAnnotation
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

@SimpleStateDiagram[WashingMachineState](
  "Washing Machine State",
  List(
    SimpleState[WashingMachineState]("on")
  ),
  List(
    SimpleState[WashingMachineState]("off"),
    SimpleState[WashingMachineState]("on")
  )
)
sealed trait WashingMachineState
case class WashingMachineStateOff() extends WashingMachineState
case class WashingMachineStateOn() extends WashingMachineState

class AParser {
  import u._
  def parse[StateAnnotation, TransitionAnnotation](staticClass: String)(implicit sttag: TypeTag[StateAnnotation], tttag: TypeTag[TransitionAnnotation]): Unit = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val stateAnnotation: Option[StateAnnotation] = annotatedClass.annotations.collectFirst{ case s: SANNO => s }
    val methodAnnotations: List[TransitionAnnotation] = annotatedClass.info.decls.flatMap(_.annotations.collect{ case t: TANNO => t }).toList
    val subclasses: Set[Symbol] = annotatedClass.knownDirectSubclasses

    println(s"stateAnnotation = $stateAnnotation, and transitionAnnotation = $methodAnnotations")
    println(s"stateAnnotation.getClass = ${stateAnnotation.getClass}, and methods.getClass = ${methodAnnotations.getClass}")
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

  def mapDebug[A](va: A):A = {
    println(s"mapDebug $va")
    va
  }

  def simpleParse[A](staticClass: String)(implicit sttag: TypeTag[SimpleStateDiagram[A]]) = {
    getClassSymbol(staticClass)
      .map(_.annotations.find(isExpectedAnnotation[SimpleStateDiagram[A]]))
      .flatMap {
        case Some(ann) => Success(ann)
        case None =>  Failure(new IllegalArgumentException("Provided class is not annotated"))
      }
      .map[List[Any]](_.tree.children.tail)
      .map {
        case _ @ Literal(Constant(nnn)) :: i :: s :: _ => (i, s) match {
          case (ii: Tree, ss: Tree) => {
            println()
//            println(s"name $nnn, matched with $ii, $ss")
            println()
            println()
//            ii.productIterator
//              .toList.tail
//              .flatMap(_.asInstanceOf[List[SimpleState[A]]])
//              .map(_=>SimpleState[A]("xx"))
//              .map(_.name)
//              .foreach(println)
            println()
            println()

            ii.productIterator
              .toList.tail
              .map {
                case Apply(Select(Ident(TermName(tn1)), TermName("$plus")), List(Literal(Constant(x)))) => println(s"got x = $x")
                case a => println(s"not matched with $a")
              }

            println()
//            (ii.produc, ss.children.tail) match {
//              case (_ @ Literal(Constant(iii)) :: _, _ @ Literal(Constant(sss)) :: _) => {
//                println(s"constant $iii, and $sss")
//              }
//              case (iii, sss) => {
//                println(s"xxx with $iii, $sss")
//                println(Console.RED + s"class analyse = ${iii.getClass}, ${sss.getClass}" + Console.RESET)
//              }
//            }
          }
          case _ => println(s"not matched")
        }
        case _ => println(s"unable to parse literal")
      }
//      .map {
//        case n :: i :: s :: _ => {
//          println(s"Found $n, $i, $s")
//          println(s"typeOf ${n.getClass}")
//          println(s"typeOf ${i.getClass}")
//          println(s"typeOf ${s.getClass}")
//
//          (n, i, s) match {
//            case (_ @ Literal(Constant(nnn)), ii: Tree, ss: Tree ) => {
//              println(s"nnn = $nnn")
//              println(s"typeOf nnn = ${nnn.getClass}")
//              println(s"ii = ${ii.children}")
//              println(s"ss = ${ss.children}")
////              SimpleState[A](nnn, ii.children.tail)
//            }
//          }
//        }
//        case _ =>
//          println("not found")
//      }
//      .map { _.tree.children.tail match {
//        case
//          Literal(Constant(name: String)) ::
//          Literal(Constant(initial: scala.collection.immutable.List[SimpleState[A]])) ::
//          Literal(Constant(states: scala.collection.immutable.List[SimpleState[A]])) :: _ => {
//          println(Console.RED + "got matched" + Console.RESET)
//          SimpleStateDiagram(name, initial, states)
//        }
//        case n :: i :: s :: _ => {
//          val name = n.value.asInstanceOf[String]
//          val initial = i.asInstanceOf[List[SimpleState[A]]]
//          val states = s.asInstanceOf[List[SimpleState[A]]]
//
//          SimpleStateDiagram[A](name, initial, states)
//        }
//        case x => {
//          println(Console.RED + s"not matched with $x" + Console.RESET)
//          SimpleStateDiagram("", Nil, Nil)
//        }
//      }}
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


object Sandbox2 extends App {
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

  val tpe: u.Type = u.typeOf[WashingMachineState]



  // Simple Parse
  val simRes = parser.simpleParse[WashingMachineState]("com.nat.scalafsm.annotator.WashingMachineState")
  println(s"simRes = $simRes")
}
