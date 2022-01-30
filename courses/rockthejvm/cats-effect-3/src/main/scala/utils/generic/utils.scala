package utils.generic

import cats.Applicative
import cats.Functor
import cats.syntax.functor.*
import cats.syntax.applicative.*

import scala.concurrent.duration.FiniteDuration
import cats.effect.kernel.MonadCancel

extension [F[_], A](fa: F[A])
  def debug(using Functor[F]): F[A] =
    fa.map { a =>
      println(s"[${Thread.currentThread.getName}] $a")
      a
    }

def unsafeSleep[F[_]](duration: FiniteDuration)(using A: Applicative[F]): F[Unit] =
  A.pure(Thread.sleep(duration.toMillis))
