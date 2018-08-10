package catz.typeclasses.comparable

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.long._
import cats.instances.string._
import cats.syntax.eq._
import catz.typeclasses.comparable.Cats._

object EqApp extends App {

  object DateInstances {
    implicit val dateEq: Eq[Date] = Eq.instance {
      (d1, d2) => d1.getTime === d2.getTime
    }
  }

  import DateInstances._

  val d1 = new Date()
  val d2 = new Date(d1.getTime + 1000L)

  println(d1 === d1)
  println(d1 =!= d2)

  object CatInstances {
    implicit val catEq: Eq[Cat] = Eq.instance {
      (c1, c2) => c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
    }
  }

  import CatInstances._

  println(murzik === murzik)
  println(murzik =!= vasya)
}
