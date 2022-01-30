package polymorphic

import cats.effect.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.kernel.Outcome.Canceled
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Succeeded
import utils.generic.*
import scala.concurrent.duration.*

object PolymorphicFibers extends IOApp.Simple:

  // Spawn = create fibers for anything
  trait CustomGenSpawn[F[_], E] extends MonadCancel[F, E]:
    def start[A](fa: F[A]): Fiber[F, Throwable, A] // creates a fiber
    def never[A]: F[A]                             // forever suspending effect
    def cede: F[Unit]                              // a "yield" effect
    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[
      (Outcome[F, E, A], Fiber[F, E, B]),
      (Fiber[F, E, A], Outcome[F, E, B])
    ]]

  trait CustomSpawn[F[_]] extends CustomGenSpawn[F, Throwable]

  // pure, map, flatMap, raiseError, uncancellable, start

  val spawnIO = Spawn[IO] // fetch the given Spawn[IO]

  def runOnSomeThread[A](ioa: IO[A]) =
    for
      fib    <- spawnIO.start(ioa) // io.start assumes the presence of a Spawn[IO]
      result <- fib.join
    yield result

  def effectOnSomeThread[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] =
    for
      fib    <- spawn.start(fa)
      result <- fib.join
    yield result

  val meaningOfLifeOnFiber        = runOnSomeThread(IO(42))
  val anotherMeaningOfLifeOnFiber = effectOnSomeThread(IO(42))

  // to use .start extension method:
  import cats.effect.syntax.spawn.*

  // exercise - generalize code
  def genericRaceUsingSpawn[F[_], A, B](fa: F[A], fb: F[B])(using
      spawn: Spawn[F]
  ): F[Either[A, B]] =
    spawn.racePair(fa, fb).flatMap {
      case Left((outcomeA, fiberB))  =>
        outcomeA match
          case Succeeded(a) => fiberB.cancel >> a.map(Left(_))
          case Errored(e)   => fiberB.cancel >> spawn.raiseError(e)
          case Canceled()   =>
            fiberB.join.flatMap {
              case Succeeded(b) => b.map(Right(_))
              case Errored(e)   => spawn.raiseError(e)
              case Canceled()   =>
                spawn.raiseError(RuntimeException("Both computations were cancelled"))
            }
      case Right((fiberA, outcomeB)) =>
        outcomeB match
          case Succeeded(b) => fiberA.cancel >> b.map(Right(_))
          case Errored(e)   => fiberA.cancel >> spawn.raiseError(e)
          case Canceled()   =>
            fiberA.join.flatMap {
              case Succeeded(a) => a.map(Left(_))
              case Errored(e)   => spawn.raiseError(e)
              case Canceled()   =>
                spawn.raiseError(RuntimeException("Both computations were cancelled"))
            }
    }

  override def run =
    val effect1 = IO.sleep(1.second) >> IO.println("first effect").debug
    val effect2 = IO.sleep(2.seconds) >> IO.println("second effect").debug
    genericRaceUsingSpawn(effect1, effect2).void
