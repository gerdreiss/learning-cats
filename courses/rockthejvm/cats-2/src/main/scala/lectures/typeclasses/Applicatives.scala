package lectures.typeclasses

import cats.Applicative
import cats.data.Validated
import cats.syntax.applicative.*
import cats.syntax.apply.*

object Applicatives extends App:

  val twoList  = Applicative[List].pure(2)   // List(2)
  val maybeTwo = Applicative[Option].pure(2) // Some(2)

  // pure extension method
  val threeList  = 3.pure[List]
  val maybeThree = 3.pure[Option]

  // Monads extend Applicatives
  // Applicatives extend Functors
  type ErrorsOr[T] = Validated[List[String], T]
  val valid42: ErrorsOr[Int] = Validated.valid(42)
  // val valid43: ErrorsOr[Int]  = 43.pure[Validated]
  val mapped43: ErrorsOr[Int] = valid42.map(_ + 1)

  println(mapped43)

  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(using A: Applicative[F]): F[(A, B)] =
    // A.map2(fa, fb)((a, b) => (a, b))
    val ff: F[B => (A, B)] = A.map(fa)((a: A) => (b: B) => (a, b))
    // A.ap(ff)(fb)
    ff <*> fb

  // Applicatives can implement product from Semigroupal
  // ap(map(fa)(a => (b: B) => (a, b)))(fb)
  // Applicatives extend Semigroupal

  println(productWithApplicatives(List(1, 2, 3), List("a", "b", "c")))
