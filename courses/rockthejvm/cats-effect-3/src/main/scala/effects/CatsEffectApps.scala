package effects

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.unsafe.implicits.global

val program = for
  line <- IO(io.StdIn.readLine)
  _    <- IO.println(s"Read from console: $line")
yield ()

object CatsEffectApps extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)

object SimpleCatsEffectApps extends IOApp.Simple:
  override def run: IO[Unit] = program
