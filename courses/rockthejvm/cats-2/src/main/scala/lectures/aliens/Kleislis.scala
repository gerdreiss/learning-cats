package lectures.aliens

import cats.Id
import cats.data.Kleisli
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.compose

object Kleislis extends App:

  val f1: Int => Option[String] = x => if x % 2 == 0 then x.toString.some else none[String]
  val f2: Int => Option[Int]    = x => Some(x * 3)

  val f1K: Kleisli[Option, Int, String] = Kleisli(f1)
  val f2K: Kleisli[Option, Int, Int]    = Kleisli(f2)

  val result: Kleisli[Option, Int, String] = f2K andThen f1K
  println(result.run(2))

  val multiplyK: Kleisli[Option, Int, Int] = f2K.map(_ * 2)
  println(multiplyK.run(2))

  val chainK: Kleisli[Option, Int, String] = multiplyK.flatMap(_ => f1K)
  println(chainK.run(2))

  type InterestingKleisli[A, B] = Kleisli[Id, A, B]

  val times2 = Kleisli[Id, Int, Int](_ * 2) // = Reader[Int, Int]
  val plus4  = Kleisli[Id, Int, Int](_ + 4) // = Reader[Int, Int]

  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))

  val forred = for
    t2 <- times2
    p4 <- plus4
  yield t2 + p4

  println(composed(2))
  println(forred(2))
