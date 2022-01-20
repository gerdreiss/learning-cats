package lectures.aliens

import cats.kernel.Monoid
import cats.Contravariant
import cats.Show
import cats.syntax.show.*
import cats.syntax.contravariant.*
import cats.syntax.try_.*
import scala.util.Try

object ContravariantFunctors extends App:

  trait Format[A]:
    self =>
    def format(value: A): String
    def contramap[B](f: B => A): Format[B] =
      new:
        override def format(value: B) = self.format(f(value))

  def format[A](value: A)(using F: Format[A]) = F.format(value)

  given Format[String] with
    def format(value: String): String = s""""$value""""

  given Format[Int] with
    def format(value: Int): String = value.toString

  given Format[Boolean] with
    def format(value: Boolean): String = if value then "Y" else "N"

  given optionFormat[T](using F: Format[T], M: Monoid[T]): Format[Option[T]] =
    F.contramap[Option[T]](_.getOrElse(M.empty))

  // that doesn't make much sense, but as experiment we gonna do that anyway
  given Monoid[Boolean] with
    def empty: Boolean                           = false
    def combine(l: Boolean, r: Boolean): Boolean = l && r

  println(format(Option("some string")))
  println(format(Option.empty[String]))
  println(format(Option.empty[Int]))
  println(format(Option.empty[Boolean]))

  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(Show[Int])(_.getOrElse(0))
  // val showTry: Show[Try[Int]]       = Contravariant[Try].contramap(Try[Int])(_.getOrElse(0))

  val showOptionSyntax: Show[Option[Int]] = Show[Int].contramap(_.getOrElse(0))
  val showListSyntax: Show[List[Int]] =
    new:
      def show(nums: List[Int]): String = nums.mkString(",")

  println((1 to 10).toList.show)
