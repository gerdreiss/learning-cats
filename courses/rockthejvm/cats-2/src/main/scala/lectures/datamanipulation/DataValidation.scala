package lectures.datamanipulation

import cats.Monoid
import cats.data.Validated
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import scala.util.Try

object DataValidation extends App:

  val validValue: Validated[String, Int]   = Validated.valid(1)                 // eqv. 'right'
  val invalidValue: Validated[String, Int] = Validated.invalid("invalid value") // eqv. 'left'
  val test: Validated[String, Int]         = Validated.cond(42 < 100, 99, "invalid value")

  extension (n: Int)
    def isPrime: Boolean       = n > 1 && !((2 until n - 1) exists (n % _ == 0))
    def isNonNegative: Boolean = n >= 0
    def isLtOrEq100: Boolean   = n <= 100
    def isEven: Boolean        = n % 2 == 0

  println(2.isPrime)
  println(20.isPrime)
  println(-10.isPrime)

  /** Validate number using Either
    *   - n must be a prime
    *   - n must be non-negative
    *   - n <= 100
    *   - n must be even
    */
  def validateNumberViaEither(n: Int): Either[List[String], Int] =
    List(
      Either.cond(n.isPrime, n, s"$n is not prime"),
      Either.cond(n.isNonNegative, n, s"$n is negative"),
      Either.cond(n.isLtOrEq100, n, s"$n is greater than 100"),
      Either.cond(n.isEven, n, s"$n is not even")
    ).filter(_.isLeft)
      .map(_.swap)
      .sequence
      .filterOrElse(_.nonEmpty, n)
      .swap

  def validateNumberViaValidated(n: Int): Validated[List[String], Int] =
    // we have to override the standard Monoid instance for ints
    // because otherwise we'll get 4 x n further below, if all validations are successful
    given Monoid[Int] with
      def empty: Int                   = 0
      def combine(x: Int, y: Int): Int = x

    Validated.cond(n.isPrime, n, List(s"$n is not prime")) |+|
      Validated.cond(n.isNonNegative, n, List(s"$n is negative")) |+|
      Validated.cond(n.isLtOrEq100, n, List(s"$n is greater than 100")) |+|
      Validated.cond(n.isEven, n, List(s"$n is not even"))

  println(validateNumberViaEither(201))
  println(validateNumberViaValidated(201))

  // some usage examples
  val andThenned  = validValue.andThen(_ => invalidValue)
  val ensured     = validValue.ensure(List("invalid value"))(_ % 2 == 0)
  val transformed = validValue.map(_ + 1)
  val bimapped    = validValue.bimap(_.toUpperCase, _ + 1)

  val eitherToValidated: Validated[List[String], Int] =
    Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] =
    Validated.fromOption(Some(42), List("missing value"))
  val tryToValidated: Validated[Throwable, Int] =
    Validated.fromTry(Try("a".toInt))

  validValue.toOption
  validValue.toEither
