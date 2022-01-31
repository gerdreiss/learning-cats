package polymorphic

import utils.generic.*
import cats.*
import cats.effect.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import cats.effect.syntax.spawn.*
import cats.effect.syntax.monadCancel.*
import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import scala.util.Random
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

abstract class Mutex[F[_]]:
  def acquire: F[Unit]
  def release: F[Unit]

object Mutex:

  type Signal[F[_]] = Deferred[F, Unit]

  case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

  def _unlocked[F[_]] = State[F](locked = false, Queue.empty)

  def _create[F[_]](state: Ref[F, State[F]])(using C: Concurrent[F]): Mutex[F] =
    new:
      override def acquire: F[Unit] =
        C.uncancelable { poll =>
          C.deferred[Unit].flatMap { signal =>

            val cleanup = state.modify { s =>
              s.copy(waiting = s.waiting.filterNot(_ eq signal)) -> release
            }.flatten

            state.modify {
              case State(false, _)    =>
                State(locked = true, Queue.empty) -> C.unit
              case State(true, queue) =>
                State(locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
            }.flatten
          }
        }

      override def release: F[Unit] =
        state.modify { state =>
          if state.locked && state.waiting.nonEmpty then
            val (signal, rest) = state.waiting.dequeue
            State(locked = true, rest) -> signal.complete(()).void
          else _unlocked -> C.unit
        }.flatten

  def create[F[_]](using C: Concurrent[F]): F[Mutex[F]] =
    C.ref(_unlocked).map(_create)

end Mutex

object MutexDemo extends IOApp.Simple:

  def criticalTask[F[_]: Applicative: FlatMap]: F[Int] =
    unsafeSleep(1.second) >> Random.nextInt(100).pure

  def createLockingTask[F[_]: Monad](id: Int, mutex: Mutex[F]): F[Int] =
    for
      _   <- s"[task $id] waiting for permission...".pure.debug
      _   <- mutex.acquire // this will block if the mutex has been acquired by some other fiber
      // critical section
      _   <- s"[task $id] working...".pure.debug
      res <- criticalTask
      _   <- s"[task $id] got result: $res".pure.debug
      // critical section end
      _   <- mutex.release
      _   <- s"[task $id] lock removed.".pure.debug
    yield res

  def createCancellingTask[F[_]: Concurrent](id: Int, mutex: Mutex[F]): F[Int] =
    if id % 2 == 0 then createLockingTask(id, mutex)
    else
      for
        fib    <- createLockingTask(id, mutex)
                    .onCancel(s"[task $id] received cancellation!".pure.debug.void)
                    .start
        _      <- unsafeSleep(2.seconds) >> fib.cancel
        out    <- fib.join
        result <- out match
                    case Succeeded(effect) => effect
                    case Errored(_)        => -1.pure
                    case Canceled()        => -2.pure
      yield result

  def demoCancellingTask[F[_]: Concurrent: Parallel]: F[List[Int]] =
    for
      mutex   <- Mutex.create
      results <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
    yield results

  override def run = demoCancellingTask[IO].debug.void
