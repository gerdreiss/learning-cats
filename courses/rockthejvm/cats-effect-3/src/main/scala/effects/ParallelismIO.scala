package effects

import cats.*
import cats.effect.*
import cats.effect.implicits.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.parallel.*

object ParallelismIO extends IOApp.Simple:

  val anisIO   = IO(s"[${Thread.currentThread.getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread.getName}] Kamran")

  val composedIO = for
    ani    <- anisIO
    kamran <- kamranIO
  yield s"$ani and $kamran love Rock the JVM"

  extension [A](ioa: IO[A])
    def debug: IO[A] =
      // for
      //   a <- ioa
      //   _ <- IO.println(s"[${Thread.currentThread.getName}] $a")
      // yield a
      ioa.flatMap(a => IO.println(s"[${Thread.currentThread.getName}] $a").as(a))

  val fail = IO.raiseError(java.lang.RuntimeException("meh")).debug

  val meaningOfLife = IO(42).debug
  val favLang       = IO("Scala").debug
  val goalInLife    = (meaningOfLife, favLang).mapN((m, l) => s"Life goals: $m and $l")

  val meaningOfLifePar: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife)
  val favLangPar: IO.Par[String]    = Parallel[IO].parallel(favLang)
  val goalInLifePar: IO.Par[String] =
    (meaningOfLifePar, favLangPar).mapN((m, l) => s"Life goals: $m and $l")
  val goalInLifeSeq                 = Parallel[IO].sequential(goalInLifePar)

  // shorthand
  val goalInLifeFinal = (meaningOfLife, favLang).parMapN((m, l) => s"Life goals: $m and $l")

  // regarding failure
  val failAtLife =
    (meaningOfLife, favLang, fail).parMapN((m, l, f) => s"Life goals: $m and $l, or fail $f")

  val doubleFailure = (fail, fail).parMapN((f1, f2) => s"We should never see this!")

  override def run: IO[Unit] =
    doubleFailure.debug.void
