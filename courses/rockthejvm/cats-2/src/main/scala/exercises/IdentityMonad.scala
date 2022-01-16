package exercises

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import scala.annotation.tailrec

type Identity[T] = T

given Monad[Identity] with
  def pure[A](value: A): Identity[A] = value

  def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

  @tailrec
  def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] =
    f(a) match
      case Right(b) => b
      case Left(a)  => tailRecM(a)(f)

@main def identityMonad(): Unit =
  val answer = "The Answer to the Ultimate Question of Life, the Universe, and Everything"
  val id6: Identity[Int] = 6
  val id7: Identity[Int] = 7

  val ftwFM = id6
    .flatMap(_6 =>
      id7
        .map(_7 => s"$answer is ${_6 * _7}")
    )
  val ftwFor = for
    _6 <- id6
    _7 <- id7
  yield s"$answer is ${_6 * _7}"

  println(ftwFM)
  println(ftwFor)
