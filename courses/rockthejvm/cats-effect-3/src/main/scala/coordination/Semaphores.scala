package coordination

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.parallel.*
import utils.*

import scala.concurrent.duration.*
import scala.util.Random

object Semaphores extends IOApp.Simple:

  // val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // example: limiting the number of concurrent sessions on a server
  def computeNum: IO[Int] = IO.sleep(1.second) *> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] =
    for
      _   <- IO(s"[session $id] waiting to log in...").debug
      _   <- sem.acquire
      // critical section
      _   <- IO(s"[session $id] logged in, working...").debug
      res <- computeNum
      _   <- IO(s"[session $id] done: $res, logging out...").debug
      // end of critical section
      _   <- sem.release
    yield res

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] =
    for
      _   <- IO(s"[session $id] waiting to log in...").debug
      _   <- sem.acquireN(requiredPermits)
      // critical section
      _   <- IO(s"[session $id] logged in, working...").debug
      res <- computeNum
      _   <- IO(s"[session $id] done: $res, logging out...").debug
      // end of critical section
      _   <- sem.releaseN(requiredPermits)
    yield res

  def demoSemaphore: IO[Unit] =
    for
      sem  <- Semaphore[IO](2)
      fib1 <- weightedLogin(1, 1, sem).start
      fib2 <- weightedLogin(2, 2, sem).start
      fib3 <- weightedLogin(3, 3, sem).start
      _    <- fib1.join
      _    <- fib2.join
      _    <- fib3.join
    yield ()

  // exercise
  // Semaphore with 1 permit == mutex
  val users: IO[List[Int]] = Semaphore[IO](1)
    .flatMap { mutex =>
      (1 to 10).toList.parTraverse { id =>
        for
          _   <- IO(s"[session $id] waiting to log in...").debug
          _   <- mutex.acquire
          // critical section
          _   <- IO(s"[session $id] logged in, working...").debug
          res <- computeNum
          _   <- IO(s"[session $id] done: $res, logging out...").debug
          // end of critical section
          _   <- mutex.release
        yield res
      }
    }

  override def run: IO[Unit] = users.debug.void
