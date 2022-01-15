package com.github.learningcats.functors

import cats.instances.function._
import cats.syntax.functor._

object FunctionSequencing extends App {

  val f1 = (x: Int) => x.toDouble
  val f2 = (d: Double) => d + 1
  val f3 = (d: Double) => d * 2
  val f4 = (d: Double) => s"$d!"

  val funcSeq = f1 map f2 map f3 map f4

  println(funcSeq(10))
}
