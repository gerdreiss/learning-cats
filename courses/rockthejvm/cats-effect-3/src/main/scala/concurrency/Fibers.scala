package concurrency

import cats.effect.*
import cats.effect.kernel.Outcome.*
import utils.*
import cats.syntax.all.*
import scala.concurrent.duration.*
import cats.effect.unsafe.implicits.global
import java.util.concurrent.TimeoutException

object Fibers extends IOApp.Simple:

  val meaningOfLife = IO.pure(42)
  val favLang       = IO.pure("Scala")

  def sameThreadComposition: IO[(Int, String)] = for
    meaning <- meaningOfLife.debug
    lang    <- favLang.debug
  yield (meaning, lang)

  // Introducing Fibers
  def createFiber: Fiber[IO, java.lang.Throwable, String] =
    ??? // almost impossible to create yourself

  // the fiber is not actually started, but the fiber allocation is wrapper in another effect
  // type FiberIO[Int] = Fiber[IO, Throwable, Int]
  val meaningOfLifeFiber: IO[FiberIO[Int]] = meaningOfLife.debug.start

  def differentThreadComposition = for
    _ <- meaningOfLifeFiber
    _ <- favLang.debug
  yield ()

  // joining a fiber
  // type OutcomeIO[A] = Outcome[IO, Throwable, A]
  // possible outcomes:
  //   Success with an IO
  //   Failure with an Exception
  //   Cancellation
  def runOnAnotherThread[A](ioa: IO[A]): IO[OutcomeIO[A]] = for
    fib    <- ioa.debug.start
    result <- fib.join // waits for the fiber to terminate
  yield result

  val ioOnAnotherThread       = runOnAnotherThread(meaningOfLife)
  val resultFromAnotherThread = ioOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(error)    => IO.raiseError(error)
    case Canceled()        => IO(0)
  }

  def throwOnAnotherThread = for
    fib    <- IO.raiseError[Int](RuntimeException("Boom!")).start
    result <- fib.join
  yield result

  def testCancel =
    val task                        = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("cancelled").debug.void)

    for
      fib    <- taskWithCancellationHandler.start              // different thread
      _      <- IO.sleep(500.millis) >> IO("cancelling").debug // calling thread
      _      <- fib.cancel                                     // calling thread
      result <- fib.join                                       // calling thread
    yield result

  def processResultFromFiberG[A](ioa: IO[A]): IO[A] =
    runOnAnotherThread(ioa).flatMap {
      case Succeeded(effect) => effect
      case Errored(error)    => IO.raiseError(error)
      case Canceled()        => IO.raiseError(java.lang.RuntimeException("cancelled"))
    }

  def processResultFromFiberDan[A](ioa: IO[A]): IO[A] = processResultFromFiberG(ioa)

  def ioTupleG[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] =
    (for
      fibA <- ioa.start
      fibB <- iob.start
      outA <- fibA.join
      outB <- fibB.join
    yield (outA, outB))
      .flatMap {
        case (Succeeded(ioa), Succeeded(iob)) => (ioa, iob).mapN((_, _))
        case (Errored(e), _)                  => IO.raiseError(e)
        case (_, Errored(e))                  => IO.raiseError(e)
        case (Canceled(), _)                  => IO.raiseError(RuntimeException("left was cancelled"))
        case (_, Canceled())                  => IO.raiseError(RuntimeException("right was cancelled"))
      }

  def ioTupleDan[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = ioTupleG(ioa, iob)

  def timeoutG[A](ioa: IO[A], duration: FiniteDuration): IO[A] = ???

  def timeoutDan[A](ioa: IO[A], duration: FiniteDuration): IO[A] =
    (for
      fib    <- ioa.start
      _      <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    yield result)
      .flatMap {
        case Succeeded(effect) => effect
        case Errored(error)    => IO.raiseError(error)
        case Canceled()        => IO.raiseError(TimeoutException("timed out"))
      }

  override def run: IO[Unit] =
    testCancel.debug.void
