package mtl_

import cats.Applicative
import cats.Monad
import cats.data.Chain
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.mtl.Listen
import cats.mtl.syntax.listen.*
import cats.mtl.syntax.tell.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.mtl.Tell
import cats.data.Writer

object ListenF extends IOApp:

  def sendToServer[F[_]: Applicative](logs: Chain[String]): F[Unit] =
    println(show"Sending to server: $logs").pure[F]

  def sendLogsToServer[F[_]: Monad, A](
      logProgram: F[A]
  )(using Listen[F, Chain[String]]): F[A] =
    logProgram.listen.flatMap { case (a, logs) =>
      sendToServer[F](logs).as(a)
    }

  def logging[F[_]: Monad](using Tell[F, Chain[String]]): F[Unit] =
    for
      _ <- Chain.one("First log").tell[F]
      _ <- Chain.one("Second log").tell
    yield ()

  override def run(args: List[String]): IO[ExitCode] =
    val result = sendLogsToServer(logging[Writer[Chain[String], *]]).value

    IO.println(result).as(ExitCode.Success)
