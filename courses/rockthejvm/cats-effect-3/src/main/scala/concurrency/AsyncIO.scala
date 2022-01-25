package concurrency

import cats.effect.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import utils.*
import scala.concurrent.Future

object AsyncIO extends IOApp.Simple:

  // IOs can run asynchronously on fibers,
  // without having to manually manage the fiber lifecycle
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(threadpool)

  val threadpool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

  type ErrorOr[A]  = Either[Throwable, A]
  type Callback[A] = ErrorOr[A] => Unit

  def meaningOfLifeFunction =
    Thread.sleep(1000)
    println(
      s"[${Thread.currentThread.getName}] computing meaning of life on some other thread..."
    )
    42

  def computeMeaningOfLife: ErrorOr[Int] = Try(meaningOfLifeFunction).toEither

  // this is useless since we can't get hold of the result of the computation
  def computeMeaningOfLifeOnThreadpool: Unit =
    threadpool.execute(() => computeMeaningOfLife)

  // lift an async computation to an IO
  // async is a 'Foreign Function Interface - FFI'
  val asyncMeaningOfLife: IO[Int] = IO.async_ { // cats effect thread blocks (semantically)
    callback => // until this callback is invoked (by some other thread)
      threadpool.execute { () => // computation not management by cats effect
        val result = computeMeaningOfLife
        callback(result) // cats effect thread is notified with the result
      }
  }

  // generalize the above

  // mine doesn't work
  def asyncToIO_G[A](computation: () => A)(using ec: ExecutionContext): IO[A] =
    IO.async_ { callback =>
      for result <- IO(computation()).evalOn(ec).debug.attempt
      yield callback(result)
    }

  def asyncToIO_Dan[A](computation: () => A)(using ec: ExecutionContext): IO[A] =
    IO.async_ { callback =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        callback(result)
      }
    }

  // lift a future into an IO
  lazy val computeMeaningOfLifeFuture: Future[Int]   = Future(meaningOfLifeFunction)
  lazy val computeMeaningOfLifeIOFromFuture: IO[Int] = IO.fromFuture(IO(computeMeaningOfLifeFuture))

  def convertFutureToIOViaAsync_G[A](future: => Future[A]): IO[A] =
    IO.async_ { callback =>
      future.onComplete(r => callback(r.toEither))
    }

  def convertFutureToIOViaAsync_Dan[A](future: => Future[A]): IO[A] =
    convertFutureToIOViaAsync_G(future)

  // a never ending IO
  val neverEndingIOAsync: IO[Unit] = IO.async_(_ => ())
  val neverEndingIO: IO[Unit]      = IO.never

  /**
   * FULL FUNC CALL
   */
  def demoAsyncCancellation =
    val finalizer: Option[IO[Unit]]   = Some(IO("Cancelled").debug.void)
    val asyncMeaningOfLifeIO: IO[Int] = IO.async { (callback: Callback[Int]) =>
      /**
           * finalizer in case a computation gets cancelled
           * finalizers are of type IO[Unit]
           * not specifying a finalizer => Option[IO[Unit]]
           * creating Option is an effect, so we need an effect => IO[Option[IO[Unit]]]
           */
      // we have to return IO[Option[IO[Unit]]]
      IO {
        threadpool.execute { () =>
          val result = Try {
            Thread.sleep(1000)
            println(
              s"[${Thread.currentThread.getName}] computing meaning of life on some other thread..."
            )
            42
          }.toEither

          callback(result)
        }
      }.as(finalizer)
    }

    for
      fib <- asyncMeaningOfLifeIO.start
      _   <- IO.sleep(500.millis) >> IO("Cancelling...").debug >> fib.cancel
      _   <- fib.join
    yield ()

  override def run =
    // asyncMeaningOfLife.debug >> IO(threadpool.shutdown())
    // asyncToIO_G(() => meaningOfLifeFunction)(ec).debug.void
    // asyncToIO_Dan(() => meaningOfLifeFunction).debug.void
    demoAsyncCancellation.debug >> IO(threadpool.shutdown)
