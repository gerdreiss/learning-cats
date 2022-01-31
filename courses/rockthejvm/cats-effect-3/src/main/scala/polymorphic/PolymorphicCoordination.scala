package polymorphic

import cats.effect.*
import cats.effect.syntax.monadCancel.*
import cats.effect.syntax.spawn.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import utils.generic.*

import scala.concurrent.duration.*

object PolymorphicCoordination extends IOApp.Simple:

  trait CustomConcurrent[F[_]] extends Spawn[F]:
    def ref[A](a: A): Ref[F, A]
    def deferred[A]: F[Deferred[F, A]]

  val concurrentIO = Concurrent[IO]
  // val deferredIO   = Deferred[IO, Int]
  val deferredIO   = concurrentIO.deferred[Int]
  val refIO        = concurrentIO.ref(42)

  // capabilities: pure, map/flatMap, raiseError, uncancellable, start (fibers), ref/deferred
  def polymorphicEggBoiler[F[_]](using C: Concurrent[F]): F[Unit] =
    def eggReadyNotification(signal: Deferred[F, Unit]) =
      for
        _ <- C.pure("Egg boiling on some other fiber, waiting...").debug
        _ <- signal.get
        _ <- C.pure("EGG READY!").debug
      yield ()

    def tickingClock(counter: Ref[F, Int], signal: Deferred[F, Unit])(using
        C: Concurrent[F]
    ): F[Unit] =
      for
        _     <- unsafeSleep(1.second)
        count <- counter.updateAndGet(_ + 1)
        _     <- C.pure(count).debug
        _     <- if count >= 10 then signal.complete(()).void else tickingClock(counter, signal)
      yield ()

    for
      counter         <- C.ref(0)
      signal          <- C.deferred[Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock           <- tickingClock(counter, signal).start
      _               <- notificationFib.join
      _               <- clock.join
    yield ()

  // generalize racePair
  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // (winner result, loser fiber)
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B])  // (loser fiber, winner result)
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  def racePair[F[_], A, B](fa: F[A], fb: F[B])(using C: Concurrent[F]): F[RaceResult[F, A, B]] =
    C.uncancelable { poll =>
      for
        signal <- C.deferred[EitherOutcome[F, A, B]]
        fibA   <- fa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
        fibB   <- fb.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
        result <- poll(signal.get) // Blocking call - should be cancellable
                    .onCancel {
                      for
                        cancelFibA <- fibA.cancel.start
                        cancelFibB <- fibB.cancel.start
                        _          <- cancelFibA.join
                        _          <- cancelFibB.join
                      yield ()
                    }
      yield result match
        case Left(outcomeA)  => Left((outcomeA, fibB))
        case Right(outcomeB) => Right((fibA, outcomeB))
    }

  override def run = polymorphicEggBoiler
