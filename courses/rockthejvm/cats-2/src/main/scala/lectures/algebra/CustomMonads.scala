package lectures.algebra

import cats.Monad

import scala.annotation.tailrec

trait Maybe[+T]:
  def get: T

case object Nope extends Maybe[Nothing]:
  def get: Nothing = throw new NoSuchElementException("Nope.get")

case class Yup[+T](value: T) extends Maybe[T]:
  def get: T = value

object Maybe:
  def nope[A]: Maybe[A] = Nope
  def yup[A](value: A): Maybe[A] = Yup(value)

given Monad[Maybe] with
  def pure[A](value: A): Maybe[A] = Yup(value)

  def flatMap[A, B](maybeA: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
    maybeA match
      case Yup(a) => f(a)
      case Nope   => Nope

  @tailrec
  def tailRecM[A, B](a: A)(f: A => Maybe[Either[A, B]]): Maybe[B] =
    f(a) match
      case Nope          => Nope
      case Yup(Right(b)) => Yup(b)
      case Yup(Left(a))  => tailRecM(a)(f)

@main def customMonads(): Unit = ???
