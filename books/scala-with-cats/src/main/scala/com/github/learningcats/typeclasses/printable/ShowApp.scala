package com.github.learningcats.typeclasses.printable

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import Cats._

object ShowApp extends App {

  implicit val catShow: Show[Cat] =
    Show.show { a =>
      val name = a.name.show
      val age = a.age.show
      val color = a.color.show
      s"$name is a $age year-old $color cat."
    }

  println(murzik.show)
}
