package polymorphic

import cats.effect.*
import cats.syntax.all.*
import cats.effect.syntax.all.*
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

  override def run = polymorphicEggBoiler
