package polymorphic

import cats.effect.*
import utils.generic.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object PolymorphicAsync extends IOApp.Simple:

  trait CustomAsync[F[_]] extends Sync[F] with Temporal[F]:
    def executionContext: F[ExecutionContext]
    def async[A](callback: (Either[Throwable, A] => Unit) => IO[Option[IO[Unit]]]): F[A]
    def async_[A](callback: (Either[Throwable, A] => Unit) => Unit): F[A]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]
    def never[A]: F[A] // never-ending computation

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

  override def run = ???
