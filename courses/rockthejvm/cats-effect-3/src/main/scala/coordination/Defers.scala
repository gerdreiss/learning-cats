package coordination

import cats.effect.*
import cats.syntax.traverse.*
import utils.*

import scala.concurrent.duration.*

object Defers extends IOApp.Simple:

  val deferredIO: IO[Deferred[IO, Int]] = Deferred[IO, Int] // or IO.deferred[Int]

  // .get blocks the calling fiber (semantically)
  // until some other fiber completes the Deferred with a value

  val writer: IO[Boolean] = deferredIO.flatMap(_.complete(42))
  // blocks until the writer has written to the deferred
  val reader: IO[Int]     = deferredIO.flatMap(_.get)

  def demoDeferred: IO[Unit] =
    def producer(signal: Deferred[IO, Int]) =
      for
        _   <- IO("[producer] crunching numbers...").debug
        _   <- IO.sleep(1.second)
        msg <- IO(42)
        _   <- IO(s"[producer] complete: $msg").debug
        _   <- signal.complete(msg)
      yield ()

    def consumer(signal: Deferred[IO, Int]) =
      for
        _   <- IO("[consumer] waiting for result...").debug
        msg <- signal.get // blocker
        _   <- IO(s"[consumer] got the result: $msg").debug
      yield ()

    for
      signal <- IO.deferred[Int]
      cons   <- consumer(signal).start
      prod   <- producer(signal).start
      _      <- prod.join
      _      <- cons.join
    yield ()

  // simulate downloading some content
  val fileParts = List("some ", "parts ", "of ", "a ", "file", "<EOF>")

  def fileNotifierWithRef: IO[Unit] =
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"[downloader] got '$part'.").debug >>
            IO.sleep(1.second) >>
            contentRef.update(_ + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] =
      for
        file <- contentRef.get
        _    <- if file.endsWith("<EOF>") then IO("[notifier] file download complete.").debug
                else
                  // problem: busy waiting
                  IO("[notifier] downloading...").debug >>
                    IO.sleep(500.millis) >>
                    notifyFileComplete(contentRef)
      yield ()

    for
      contentRef <- Ref[IO].of("")
      downloader <- downloadFile(contentRef).start
      notifier   <- notifyFileComplete(contentRef).start
      _          <- downloader.join
      _          <- notifier.join
    yield ()

  // deferred works miracles for waiting
  def fileNotifierWithDeferred: IO[Unit] =
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] =
      for
        _ <- IO("[notifier] downloading...").debug
        _ <- signal.get // blocks until the signal is received
        _ <- IO("[notifier] file download complete.").debug
      yield ()

    def downloadFile(
        part: String,
        contentRef: Ref[IO, String],
        signal: Deferred[IO, String]
    ): IO[Unit] =
      for
        _       <- IO(s"[downloader] got '$part'.").debug
        _       <- IO.sleep(1.second)
        content <- contentRef.updateAndGet(_ + part)
        _       <- if content.endsWith("<EOF>") then signal.complete(content) else IO.unit
      yield ()

    for
      contentRef <- Ref[IO].of("")
      signal     <- Deferred[IO, String]
      notifier   <- notifyFileComplete(signal).start
      fileTasks  <- fileParts.map(part => downloadFile(part, contentRef, signal)).sequence.start
      _          <- notifier.join
      _          <- fileTasks.join
    yield ()

  // exercises
  // 1
  def eggBoiler: IO[Unit] =
    def eggReadyNotification(signal: Deferred[IO, Unit]): IO[Unit] =
      for
        _ <- IO("Egg boiling on some other fiber, waiting...").debug
        _ <- signal.get
        _ <- IO("Egg ready").debug
      yield ()

    def tickingClock(ticks: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] =
      for
        _     <- IO.sleep(1.second)
        count <- ticks.updateAndGet(_ + 1)
        _     <- IO(count).debug
        _     <- if count == 10 then signal.complete(()) else tickingClock(ticks, signal)
      yield ()

    for
      counter  <- Ref[IO].of(0)
      signal   <- Deferred[IO, Unit]
      notifier <- eggReadyNotification(signal).start
      clock    <- tickingClock(counter, signal).start
      _        <- notifier.join
      _        <- clock.join
    yield ()

  // 2
  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])  // (loser fiber, winner result)
  ]

  type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def racePairViaDeferred[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] =
    IO.uncancelable { poll =>
      for
        signal <- Deferred[IO, EitherOutcome[A, B]]
        fibA   <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
        fibB   <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
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

  override def run = eggBoiler
