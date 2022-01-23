package concurrency

import cats.effect.*
import cats.effect.kernel.Outcome.Canceled
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Succeeded
import utils.*

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple:

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation for $value").debug >>
        IO.sleep(duration) >>
        IO(s"computation for $value done") >>
        IO(value)
    ).onCancel(IO(s"computation for $value cancelled").debug.void)

  def testRace: IO[String] =
    val meaningOfLife: IO[Int]              = runWithSleep(42, 1.seconds)
    val favLang: IO[String]                 = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    raceResult.flatMap {
      case Left(n)  => IO(s"Meaning of life won: $n")
      case Right(l) => IO(s"Favorite language won: $l")
    }

  def testRacePair =
    val meaningOfLife: IO[Int] = runWithSleep(42, 1.seconds)
    val favLang: IO[String]    = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[
      (OutcomeIO[Int], FiberIO[String]), // (winner result, loser fiber)
      (FiberIO[Int], OutcomeIO[String])  // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)
    raceResult.flatMap {
      case Left((outcome, fiber))  =>
        fiber.cancel >> IO("Meaning of life won").debug >> IO(outcome)
      case Right((fiber, outcome)) =>
        fiber.cancel >> IO("Favorite language won").debug >> IO(outcome)
    }

  /**
   * EXERCISES
   */

  def timeoutG[A](ioa: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(ioa, IO.sleep(duration))
      .flatMap {
        case Left(a)  => IO(s"computation successful: $a").debug >> IO(a)
        case Right(e) => IO(s"computation timed out").debug >> IO.raiseError(TimeoutException())
      }

  def timeoutDan[A](ioa: IO[A], duration: FiniteDuration): IO[A] = timeoutG(ioa, duration)

  // return the losing effect
  def unraceG[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Right((fiberA, _)) =>
        IO("First effect lost").debug >>
          fiberA.join.flatMap {
            case Succeeded(a) => a.map(Left(_))
            case _            =>
              IO.raiseError[Either[A, B]](RuntimeException("First effect failed or cancelled"))
          }.debug
      case Left((_, fiberB))  =>
        IO("Second effect lost").debug >>
          fiberB.join.flatMap {
            case Succeeded(b) => b.map(Right(_))
            case _            =>
              IO.raiseError[Either[A, B]](RuntimeException("Second effect and failed or cancelled"))
          }.debug
    }

  def unraceDan[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = unraceG(ioa, iob)

  // race in terms of racePair
  // my implementation is wrong
  // it doesn't take the effect cancelling order
  def simpleRaceG[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fiberB))  =>
        fiberB.cancel >> {
          outcomeA match
            case Succeeded(a) => a.map(Left(_))
            case Errored(e)   => IO.raiseError(e)
            case Canceled()   => IO.raiseError(TimeoutException())
        }
      case Right((fiberA, outcomeB)) =>
        fiberA.cancel >> {
          outcomeB match
            case Succeeded(b) => b.map(Right(_))
            case Errored(e)   => IO.raiseError(e)
            case Canceled()   => IO.raiseError(TimeoutException())
        }
    }

  def simpleRaceDan[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outcomeA, fiberB))  =>
        outcomeA match
          case Succeeded(a) => fiberB.cancel >> a.map(Left(_))
          case Errored(e)   => fiberB.cancel >> IO.raiseError(e)
          case Canceled()   =>
            fiberB.join.flatMap {
              case Succeeded(b) => b.map(Right(_))
              case Errored(e)   => IO.raiseError(e)
              case Canceled()   =>
                IO.raiseError(RuntimeException("Both computations were cancelled"))
            }
      case Right((fiberA, outcomeB)) =>
        outcomeB match
          case Succeeded(b) => fiberA.cancel >> b.map(Right(_))
          case Errored(e)   => fiberA.cancel >> IO.raiseError(e)
          case Canceled()   =>
            fiberA.join.flatMap {
              case Succeeded(a) => a.map(Left(_))
              case Errored(e)   => IO.raiseError(e)
              case Canceled()   =>
                IO.raiseError(RuntimeException("Both computations were cancelled"))
            }

    }

  override def run: IO[Unit] =
    // IO.race(runWithSleep(42, 1.seconds), runWithSleep("Scala", 2.seconds)).void
    simpleRaceG(runWithSleep(42, 1.seconds), runWithSleep("Scala", 2.seconds)).void
