package effects

import cats.effect.IO
import cats.effect.IOApp
import cats.effect.unsafe.implicits.global
import cats.syntax.flatMap.*

object Effects extends IOApp.Simple:

  val clock: IO[Long] = IO(System.currentTimeMillis)

  def measure[A](computation: IO[A]): IO[Long] =
    for
      start  <- clock
      _      <- computation
      finish <- clock
    yield finish - start

  def putStrLn(text: String): IO[Unit] =
    IO(println(text))

  val readLn: IO[String] =
    IO(io.StdIn.readLine)

  val askUsername =
    for
      _    <- putStrLn("Hello, what's your name")
      name <- readLn
      _    <- putStrLn("Hello, " + name)
    yield ()

  val program =
    for
      time <- measure(askUsername)
      _    <- putStrLn(s"This operation took $time ms")
    yield ()

  def run: IO[Unit] = program
