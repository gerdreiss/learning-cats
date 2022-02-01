package polymorphic

import cats.*
import cats.effect.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import utils.generic.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object PolymorphicAsync extends IOApp.Simple:

  trait CustomAsync[F[_]] extends Sync[F] with Temporal[F]:
    def executionContext: F[ExecutionContext]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]
    def async[A](callback: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]

    def async_[A](callback: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(cb => map(pure(callback(cb)))(_ => None))

    def never[A]: F[A] = async_(_ => ())

  val asyncIO = Async[IO]

  val ec = asyncIO.executionContext

  val threadPool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

  type Callback[A] = Either[Throwable, A] => Unit

  // power: async_ + async = FFI
  val asyncMeaningOfLife: IO[Int] = IO.async_ { callback =>
    // start computation on some other thread pool
    threadPool.execute { () =>
      println(s"[${Thread.currentThread.getName}]: Computing an async meaning of life")
      callback(Right(42))
    }
  }

  val asyncMeaningOfLifeComplex: IO[Int] = IO.async { callback =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread.getName}]: Computing an async meaning of life")
        callback(Right(42))
      }
    }.as(Some(IO("cancelled!").debug.void))
  }

  // exercises
  // 1 - implement async_ and never with async
  // tuple two effects with diff. requirements

  def effect1[F[_]: Concurrent, A](a: A): F[A] = a.pure[F]
  def effect2[F[F]: Sync, A](a: A): F[A]       = a.pure[F]

  def effectTuple[F[_]: Async, A](a: A): F[(A, A)] =
    for
      eff1 <- effect1(a)
      eff2 <- effect2(a)
    yield (eff1, eff2)

  override def run = ???
