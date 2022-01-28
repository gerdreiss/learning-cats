package coordination

import cats.effect.*
import cats.effect.std.*
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import cats.syntax.functor.*
import utils.*

import java.io.File
import java.io.FileReader
import java.io.FileWriter
import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.duration.*
import scala.io.Source

object CountDownLatches extends IOApp.Simple:

  def announcer(latch: CountDownLatch[IO]): IO[Unit] =
    for
      _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
      _ <- IO("5...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("4...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("3...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("2...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("1...").debug >> IO.sleep(1.second)
      _ <- latch.release
      _ <- IO("GO GO GO!").debug
    yield ()

  def runner(id: Int, latch: CountDownLatch[IO]): IO[Unit] =
    for
      _ <- IO(s"[runner $id] waiting for signal...").debug
      _ <- latch.await
      _ <- IO(s"[runner $id] RUNNING!").debug
    yield ()

  def sprint: IO[Unit] =
    for
      latch        <- CountDownLatch[IO](5)
      announcerFib <- announcer(latch).start
      _            <- (1 to 10).toList.parTraverse(id => runner(id, latch))
      _            <- announcerFib.join
    yield ()

  // exercise: simulate file downloader on multiple threads
  object FileServer:
    private val fileChunks = Array(
      "Some ",
      "random ",
      "fictitious ",
      "imaginary ",
      "file ",
      "chunks"
    )

    def getNumberOfChunks: IO[Int]       = IO(fileChunks.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunks(n - 1))

  def writeToFile(path: Path, content: String): IO[Unit] =
    Resource
      .make(IO(FileWriter(path.toFile)))(w => IO(w.close()))
      .use(w => IO(w.write(content)))

  def appendFileContents(from: Path, to: Path): IO[Unit] =
    val compositeResource =
      for
        reader <- Resource.make(IO(Source.fromFile(from.toFile)))(r => IO(r.close()))
        writer <- Resource.make(IO(FileWriter(to.toFile, true)))(w => IO(w.close()))
      yield (reader, writer)

    compositeResource.use { (r, w) =>
      IO(r.getLines.foreach(w.write))
    }

  def downloadFileG(target: Path): IO[Unit] =
    for
      numChunks <- FileServer.getNumberOfChunks
      latch     <- CountDownLatch[IO](numChunks)
      _         <- IO(s"Download started on $numChunks fibers").debug
      _         <- (1 to numChunks).toList.parTraverse { id =>
                     for
                       _     <- IO(s"[task $id] download chunk...").debug
                       chunk <- FileServer.getFileChunk(id)
                       tmp    = Paths.get(s"${target}.part$id")
                       _     <- writeToFile(tmp, chunk)
                       _     <- IO(s"[task $id] chunk download complete").debug
                       _     <- latch.release
                     yield ()
                   }
      _         <- latch.await
      _         <- IO(s"Combining the chunks to ${target.getFileName}...").debug
      _         <- (1 to numChunks).toList.traverse { id =>
                     appendFileContents(
                       Paths.get(s"${target}.part$id"),
                       target
                     )
                   }
      _         <- IO("done.")
    yield ()

  def downloadTaskDan(
      id: Int,
      latch: CountDownLatch[IO],
      filename: String,
      folder: String
  ): IO[Unit] =
    for
      _     <- IO(s"[task $id] download chunk...").debug
      _     <- IO.sleep((util.Random.nextInt(1000)).millis)
      chunk <- FileServer.getFileChunk(id)
      _     <- writeToFile(Paths.get(s"$folder/$filename.part$id"), chunk)
      _     <- IO(s"[task $id] chunk download complete").debug
      _     <- latch.release
    yield ()

  def downloadFileDan(filename: String, folder: String): IO[Unit] =
    for
      n     <- FileServer.getNumberOfChunks
      latch <- CountDownLatch[IO](n)
      _     <- IO(s"Download started on $n fibers...").debug
      _     <- (1 to n).toList.parTraverse { id =>
                 downloadTaskDan(id, latch, filename, folder)
               }
      _     <- latch.await
      _     <- IO(s"Combining the chunks to $filename...").debug
      _     <- (1 to n).toList.traverse { id =>
                 appendFileContents(
                   Paths.get(s"$folder/$filename.part$id"),
                   Paths.get(s"$folder/$filename")
                 )
               }
      _     <- IO("done.")
    yield ()

  override def run =
    // downloadFileDan("download.txt", System.getProperty("user.dir"))
    downloadFileG(Paths.get(System.getProperty("user.dir"), "download.txt"))
