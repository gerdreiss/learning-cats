package polymorphic

import cats.*
import cats.effect.*
import cats.syntax.functor.*
import cats.Defer
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader

object PolymorphicSync extends IOApp.Simple:

  val delayedIO = IO.delay {
    println("I'm an effect!")
    42
  }

  val blockingIO = IO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  }

  trait CustomSync[F[_]] extends cats.effect.kernel.MonadCancel[F, Throwable] with Defer[F]:
    def delay[A](thunk: => A): F[A]
    def blocking[A](thunk: => A): F[A]

    def defer[A](thunk: => F[A]): F[A] =
      flatMap(delay(thunk))(identity)

  val syncIO = Sync[IO]

  val anotherDelayedIO = syncIO.delay {
    println("I'm an effect!")
    42
  }

  val deferredIO = IO.defer(delayedIO)

  // exercise - polymorphic console implementation
  trait Console[F[_]]:
    def println[A](a: A): F[Unit]
    def readLine: F[String]

  object ConsoleG:
    def create[F[_]](using S: Sync[F]): F[Console[F]] = S.pure {
      new:
        override def println[A](a: A): F[Unit] =
          S.blocking(println(a))

        override def readLine: F[String] =
          S.blocking(io.StdIn.readLine)
    }

  object ConsoleDan:
    def make[F[_]](using S: Sync[F]): F[Console[F]] =
      S.pure((System.in, System.out))
        .map { case (in, out) =>
          new Console[F]:
            override def println[A](a: A): F[Unit] = S.blocking(out.println(a))

            override def readLine: F[String] =
              val bufferedReader = new BufferedReader(new InputStreamReader(in))
              S.blocking(bufferedReader.readLine)
        }

  override def run: IO[Unit] =
    for
      console <- ConsoleDan.make[IO]
      _       <- console.println("Hi, what's your name?")
      name    <- console.readLine
      _       <- console.println(s"Hi $name")
    yield ()
