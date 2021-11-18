package com.github.learningcats

import cats.implicits._
import cats.effect._
import cats.effect.implicits._

object Main extends App {
  import cats.effect.unsafe.implicits.global

  println("Hello " |+| "Cats!")

  val result = List(
    IO("Hello"),
    IO(", "),
    IO("sequenced "),
    IO("world"),
    IO("!")
  ).parSequence
    .map(_.mkString)
    .unsafeRunSync()

  println(result)

}
