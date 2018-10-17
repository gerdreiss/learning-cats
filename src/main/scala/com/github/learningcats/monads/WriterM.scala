package com.github.learningcats.monads

import cats.Id
import cats.data.{Writer, WriterT}

object WriterM extends App {

  val times: WriterT[Id, Vector[String], Int] = Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859)
}
