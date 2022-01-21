package utils

import cats.effect.IO

extension [A](ioa: IO[A])
  def debug: IO[A] =
    for
      a <- ioa
      _ <- IO.println(s"[${Thread.currentThread.getName}] $a")
    yield a
