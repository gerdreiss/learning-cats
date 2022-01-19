package lectures.typeclasses

import cats.Applicative
import cats.Apply
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*

object WeakerApplicatives extends App:

  val apF = Apply[Option].ap(Some((x: Int) => x + 1))(Some(2))

  val maybeF: Option[Int => Int] = Some(_ + 2)
  val apFF: Option[Int]          = maybeF <*> Some(2)

  val tupleOfOptions     = (Option(1), Option(2), Option(3))
  val Some((_1, _2, _3)) = tupleOfOptions.tupled // Some((1, 2, 3))

  println(s"${_1}, ${_2}, ${_3}")

  val Some(sum) = tupleOfOptions.mapN(_ + _ + _)

  println(sum)

  def mapN[F[_], A, B, C](ab: (F[A], F[B]))(f: (A, B) => C)(using A: Apply[F]): F[C] =
    A.product(ab._1, ab._2).map((a, b) => f(a, b))

  println(mapN(Option(1), Option(2))(_ + _))
