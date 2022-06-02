package mtl_

import cats.Applicative
import cats.Monad
import cats.data.Reader
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.mtl.Ask
import cats.mtl.Local
import cats.syntax.flatMap.*
import cats.syntax.functor.*

object LocalF extends IOApp:

  def calculateContentLength[F[_]: Applicative](using F: Ask[F, String]): F[Int] =
    F.ask.map(_.length)

  def calculateModifiedContentLength[F[_]: Applicative](using F: Local[F, String]): F[Int] =
    F.local(calculateContentLength[F])("Prefix " + _)

  def both[F[_]: Monad](using F: Local[F, String]): F[(Int, Int)] =
    for
      length         <- calculateContentLength[F]
      modifiedLength <- calculateModifiedContentLength[F]
    yield (length, modifiedLength)

  override def run(args: List[String]): IO[ExitCode] =
    val res = both[Reader[String, *]].run("Hello World!")

    IO.println(res).as(ExitCode.Success)
