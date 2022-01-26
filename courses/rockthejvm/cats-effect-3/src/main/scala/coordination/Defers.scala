package coordination

import cats.effect.*
import utils.*
import scala.concurrent.duration.*
import cats.syntax.traverse.*

object Defers extends IOApp.Simple:

  val deferred: IO[Deferred[IO, Int]]   = Deferred[IO, Int]
  val deferredIO: IO[Deferred[IO, Int]] = IO.deferred[Int]

  // get blocks the calling fiber (semantically)
  // until some other fiber completes the Deferred with a value

  val writer: IO[Boolean] = deferredIO.flatMap(_.complete(42))
  // blocks until the writer has written to the deferred
  val reader: IO[Int]     = deferredIO.flatMap(_.get)

  def demoDeferred: IO[Unit] =
    def producer(signal: Deferred[IO, Int]) =
      for
        _   <- IO("[producer] crunching numbers...").debug
        _   <- IO.sleep(1.second)
        _   <- IO("[producer] complete: 42").debug
        msg <- IO(42)
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

  override def run = fileNotifierWithDeferred
