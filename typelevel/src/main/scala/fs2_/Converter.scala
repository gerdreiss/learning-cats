package fs2_

import cats.effect.{ IO, IOApp }
import fs2.{ text, Stream }
import fs2.io.file.{ Files, Path }
import java.nio.file.Paths

object Converter extends IOApp.Simple:

  val converter: Stream[IO, Unit] =
    Files[IO]
      .readAll(Path("testdata/fahrenheit.txt"))
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.trim.nonEmpty)
      .map(_.toDouble)
      .map(tmp => (tmp - 32.0) * (5.0 / 9.0))
      .map(_.toString)
      .intersperse("\n")
      .through(text.utf8.encode)
      .through(Files[IO].writeAll(Path("testdata/celsius.txt")))

  override def run: IO[Unit] =
    converter.compile.drain
