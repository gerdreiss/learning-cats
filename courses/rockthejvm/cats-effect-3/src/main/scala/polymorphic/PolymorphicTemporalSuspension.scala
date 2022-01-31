package polymorphic

import cats.effect.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import utils.generic.*

import scala.concurrent.duration.*
import java.util.concurrent.TimeoutException

object PolymorphicTemporalSuspension extends IOApp.Simple:

  // Temporal - time-blocking effects
  trait CustomTemporal[F[_]] extends Concurrent[F]:
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks this fiber for a specified time

  val temporalIO = Temporal[IO]

  val chainOfEffects =
    IO("loading...").debug *>
      IO.sleep(1.second) *>
      IO("Ready!").debug

  val anotherChainOfEffects =
    temporalIO.pure("loading...").debug *>
      temporalIO.sleep(1.second) *>
      temporalIO.pure("Ready!").debug

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using T: Temporal[F]): F[A] =
    T.race(fa, T.sleep(duration))
      .flatMap {
        case Left(a)  => s"computation successful: $a".pure.debug >> a.pure
        case Right(e) => s"computation timed out".pure.debug >> T.raiseError(TimeoutException())
      }

  override def run: IO[Unit] = ???
