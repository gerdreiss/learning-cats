package catz.typeclasses.printable

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import catz.typeclasses.printable.Cats._

object ShowApp extends App {

  object ShowInstances {
    implicit val catShow: Show[Cat] =
      Show.show { a =>
        val name = a.name.show
        val age = a.age.show
        val color = a.color.show
        s"$name is a $age year-old $color cat."
      }
  }

  import ShowInstances._

  println(murzik.show)
}
