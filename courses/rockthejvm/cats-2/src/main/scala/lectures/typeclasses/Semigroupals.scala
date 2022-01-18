package lectures.typeclasses

import cats.Monad
import cats.Semigroupal
import cats.data.Validated
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object Semigroupals extends App:

  // trait Semigroupal[F[_]]:
  //  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  val optionSemigroupal = Semigroupal[Option]
  val tupledOption      = optionSemigroupal.product(Some(123), Some("123")) // Some((123, "123"))
  val noneTuple         = optionSemigroupal.product(Some(123), None)

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
  )

  val tupledFuture = Semigroupal[Future].product(
    Future("the meaning of life"),
    Future(42)
  )

  val tupledList = Semigroupal[List].product(List(1, 2, 3), List("a", "b", "c"))

  def productUsingMonad[F[_]: Monad, A, B](fa: F[A], fb: F[B])(using M: Monad[F]): F[(A, B)] =
    M.flatMap(fa)(a => M.map(fb)(b => (a, b)))

  def productContextBoundMonad[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for
      a <- fa
      b <- fb
    yield (a, b)

  println(productUsingMonad(List(1, 2, 3), List("a", "b", "c")))
  println(productContextBoundMonad(List(1, 2, 3), List("a", "b", "c")))

  // use for semigroupals : Validated
  type ErrorsOr[T] = Validated[List[String], T]

  val invalidsCombination = Semigroupal[ErrorsOr].product(
    Validated.invalid(List("invalid something", "invalid something else")),
    Validated.invalid(List("something very invalid"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]

  val eithersCombination = Semigroupal[EitherErrorsOr].product(
    Left(List("invalid something", "invalid something else")),
    Left(List("something very invalid"))
  )

  println(invalidsCombination)
  println(eithersCombination)

  given Semigroupal[List] with
    def product[A, B](as: List[A], bs: List[B]): List[(A, B)] = as zip bs

  println(Semigroupal[List].product(List(1, 2, 3), List("a", "b", "c")))
