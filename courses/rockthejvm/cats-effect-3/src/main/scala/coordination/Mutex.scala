package coordination

import cats.effect.*
import cats.effect.kernel.Outcome.*
import cats.syntax.parallel.*
import utils.*

import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import scala.util.Random

abstract class Mutex:
  def acquire: IO[Unit]
  def release: IO[Unit]

object Mutex:
  type Signal = Deferred[IO, Unit]

  case class State(locked: Boolean, waiting: Queue[Signal])

  val unlocked = State(locked = false, Queue.empty)

  def createSimpleMutex(state: Ref[IO, State]): Mutex =
    new:
      /**
       * Change the state of the Ref:
       * - if the mutex is currently unlocked, state becomes (true, [])    
       * - if the mutex is locked, state becomes (true, queue + new signal) AND WAIT ON THAT SIGNAL
       */
      override def acquire: IO[Unit] =
        // Dan's solution
        // IO.deferred[Unit].flatMap { signal =>
        //   state.modify {
        //     case State(false, _)    => State(locked = true, Queue.empty)           -> IO.unit
        //     case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> signal.get
        //   }.flatten
        // }
        // my solution
        IO.deferred[Unit].flatMap { signal =>
          state.modify { s =>
            if s.locked then s.copy(waiting = s.waiting.enqueue(signal)) -> signal.get
            else State(locked = true, waiting = Queue.empty)             -> IO.unit
          }
        }

      /**
       * Change the state of the Ref:
       * - if the mutex is unlocked, leave the state unchanged
       * - if the mutex is locked:
       *   - if the queue is empty, unlock the mutex, i.e. state becomes (false, [])
       *   - if the queue is not empty, take a signal out fo the queue and 
       *     complete it (thereby unblocking a fiber waiting on it)
       */
      override def release: IO[Unit] =
        // Dan's solution
        // state.modify {
        //   case State(false, _)    => unlocked -> IO.unit
        //   case State(true, queue) =>
        //     if queue.isEmpty then unlocked -> IO.unit
        //     else
        //       val (signal, rest) = queue.dequeue
        //       State(locked = true, rest) -> signal.complete(()).void
        // }.flatten
        // my solution
        state.modify { s =>
          if s.locked && s.waiting.nonEmpty then
            val (signal, rest) = s.waiting.dequeue
            State(locked = true, rest) -> signal.complete(()).void
          else s -> IO.unit
        }.flatten

  def createMutexWithCancellation(state: Ref[IO, State]): Mutex =
    new:
      override def acquire: IO[Unit] = IO.uncancelable { poll =>
        IO.deferred[Unit].flatMap { signal =>
          val cleanup = state.modify { s =>
            s.copy(waiting = s.waiting.filterNot(_ eq signal)) -> release
          // case State(locked, queue) =>
          //   val newQueue = queue.filterNot(_ eq signal)
          //   State(locked, newQueue) -> release
          }.flatten

          state.modify {
            case State(false, _)    =>
              State(locked = true, Queue.empty) -> IO.unit
            case State(true, queue) =>
              State(locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
          }.flatten
        }
      }

      override def release: IO[Unit] =
        state.modify { // modify is already cancellation-aware, no need to do anything
          case State(false, _)    => unlocked -> IO.unit
          case State(true, queue) =>
            if queue.isEmpty then unlocked -> IO.unit
            else
              val (signal, rest) = queue.dequeue
              State(locked = true, rest) -> signal.complete(()).void
        }.flatten

  def create: IO[Mutex] =
    Ref[IO].of(unlocked).map(createMutexWithCancellation)

object MutexPlayground extends IOApp.Simple:

  def criticalTask: IO[Int] =
    IO.sleep(1.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] =
    for
      _   <- IO(s"[task $id] working...").debug
      res <- criticalTask
      _   <- IO(s"[task $id] got result: $res").debug
    yield res

  def demoNonLockingTasks: IO[List[Int]] =
    (1 to 10).toList.parTraverse(createNonLockingTask)

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] =
    for
      _   <- IO(s"[task $id] waiting for permission...").debug
      _   <- mutex.acquire // this will block if the mutex has been acquired by some other fiber
      // critical section
      _   <- IO(s"[task $id] working...").debug
      res <- criticalTask
      _   <- IO(s"[task $id] got result: $res").debug
      // critical section end
      _   <- mutex.release
      _   <- IO(s"[task $id] lock removed.").debug
    yield res

  def demoLockingTask =
    for
      mutex   <- Mutex.create
      results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
    yield results

  def createCancellingTask(id: Int, mutex: Mutex): IO[Int] =
    if id % 2 == 0 then createLockingTask(id, mutex)
    else
      for
        fib    <- createLockingTask(id, mutex)
                    .onCancel(IO(s"[task $id] received cancellation!").debug.void)
                    .start
        _      <- IO.sleep(2.seconds) >> fib.cancel
        out    <- fib.join
        result <- out match
                    case Succeeded(effect) => effect
                    case Errored(_)        => IO(-1)
                    case Canceled()        => IO(-2)
      yield result

  def demoCancellingTask: IO[List[Int]] =
    for
      mutex   <- Mutex.create
      results <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
    yield results

  override def run = demoCancellingTask.debug.void
