package effects

import cats.effect.*
import cats.effect.unsafe.implicits.global

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

object ErrorHandlingWithIO extends App:

  val failedIO = IO.delay[Int](throw RuntimeException("Boom!"))
  val failure  = IO.raiseError[Int](RuntimeException("Raised error"))
  val dealt    = failure.handleErrorWith[Int] { case NonFatal(e) =>
    IO.println(s"Exception $e caught and handled") *> IO(42)
  }

  val attempt: IO[Either[Throwable, Int]] = dealt.attempt
  val result: IO[String]                  = dealt.redeem(
    ex => s"Exception $ex caught and handled",
    value => s"Success: $value"
  )
  val resultEffect                        = dealt.redeemWith(
    ex => IO.println(s"Exception $ex caught and handled"),
    value => IO.println(s"Success: $value")
  )
  resultEffect.unsafeRunSync()

  def optionToIO[A](maybeA: Option[A])(ifEmpty: Throwable): IO[A] =
    maybeA match
      case Some(a) => IO(a)
      case None    => IO.raiseError(ifEmpty)

  def tryToIO[A](triedA: Try[A]): IO[A] =
    triedA match
      case Success(a) => IO(a)
      case Failure(e) => IO.raiseError(e)

  def eitherToIO[A](eitherErrorOrA: Either[Throwable, A]): IO[A] =
    eitherErrorOrA match
      case Right(a) => IO(a)
      case Left(e)  => IO.raiseError(e)

  def handleError[A](ioa: IO[A])(handler: Throwable => A): IO[A] =
    // ioa.handleError(handler)
    ioa.redeem(handler, identity)

  def handleErrorWith[A](ioa: IO[A])(handler: Throwable => IO[A]): IO[A] =
    // ioa.handleErrorWith(handler)
    ioa.redeemWith(handler, IO.pure)
