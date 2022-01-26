package coordination

import cats.effect.*
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

  def create: IO[Mutex] = Ref[IO].of(unlocked).map { state =>
    new:
      /**
       * Change the state of the Ref:
       * - if the mutex is currently unlocked, state becomes (true, [])    
       * - if the mutex is locked, state becomes (true, queue + new signal) AND WAIT ON THAT SIGNAL
       */
      override def acquire: IO[Unit] =
        IO.deferred[Unit].flatMap { signal =>
          state.modify {
            case State(false, _)    => State(locked = true, Queue.empty)           -> IO.unit
            case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> signal.get
          }.flatten
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
  }

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

  override def run = demoLockingTask.debug.void
