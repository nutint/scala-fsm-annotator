package com.nat.scalafsm.annotator


import java.io.File

import net.sourceforge.plantuml.SourceStringReader

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => u}

case class State2[A](name: String, desc: String, data: A, enabled: Boolean) extends StaticAnnotation

case class MethodAnnotation[A](name: String) extends StaticAnnotation

@State2[Int]("aa", "description",  1,true)
@State2[String]("aa", "description",  "str",true)
case class TestClass(x: String) {
  @MethodAnnotation[Int]("foo")
  def foo(param: String) = param
}

class AParser {
  import u._
  def parse[SANNO, TANNO](staticClass: String)(implicit sttag: TypeTag[SANNO], tttag: TypeTag[TANNO]): Unit = {
    val annotatedClass: ClassSymbol = runtimeMirror(Thread.currentThread().getContextClassLoader).staticClass(staticClass)
    val stateAnnotation: Option[SANNO] = annotatedClass.annotations.collectFirst{ case s: SANNO => s }
    val methodAnnotations: List[TANNO] = annotatedClass.info.decls.flatMap(_.annotations.collect{ case t: TANNO => t }).toList
    val subclasses: Set[Symbol] = annotatedClass.knownDirectSubclasses

    println(s"stateAnnotation = $stateAnnotation, and transitionAnnotation = $methodAnnotations")
    println(s"stateAnnotation.getClass = ${stateAnnotation.getClass}, and methods.getClass = ${methodAnnotations.getClass}")
    stateAnnotation
      .map {
        // case s: StaticAnnotation => println("Found static annotation")
        case s: SANNO => println("state2")
        case _ => println("not found any annotation")
      }

    methodAnnotations
      .map {
        case m: TANNO => println("found Transition")
        case _ => println("not found transition annotation")
      }
  }

  def isExpectedAnnotation[EXA](annotation: Annotation)(implicit ttag: TypeTag[EXA]): Boolean = {
    annotation.tree.tpe =:= typeOf[EXA]
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


//  var source = "@startuml\n"
//      source += "Bob -> Alice : hello\n"
//      source += "@enduml\n"
//  val reader: SourceStringReader = new SourceStringReader(source)
//  reader.generateImage(new File("/Users/nat/Documents/bobalice.png"))
}
