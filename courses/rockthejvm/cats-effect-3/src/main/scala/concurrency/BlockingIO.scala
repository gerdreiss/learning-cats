package concurrency

import cats.effect.*
import utils.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object BlockingIO extends IOApp.Simple:

  val someSleeps =
    for
      _ <- IO.sleep(1.second).debug // semantic blocking
      _ <- IO.sleep(1.second).debug // semantic blocking
    yield ()

  // really blocking IOs
  val blockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread.getName}] computed a blocking code")
  }

  // yielding
  val iosOnManyThreads =
    for
      _ <- IO("first").debug
      _ <- IO.cede // signal to yield control over the thread, eq. to IO.shift in Cats Effect 2
      _ <- IO("second").debug
      _ <- IO.cede // signal to yield control over the thread, eq. to IO.shift in Cats Effect 2
      _ <- IO("third").debug
    yield ()

  def testThousandEffectSwitch =
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    )

    (1 to 1000)
      .map(IO.pure)
      .reduce(_.debug >> IO.cede >> _.debug)
      .evalOn(ec)

  // blocking calls & IO.sleep yield control over the calling thread automatically

  override def run = testThousandEffectSwitch.void
