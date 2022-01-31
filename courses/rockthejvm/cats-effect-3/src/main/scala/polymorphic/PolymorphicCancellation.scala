package polymorphic

import cats.*
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.kernel.Outcome.*
import cats.implicits.*
import utils.generic.*

import scala.concurrent.duration.*

object PolymorphicCancellation extends IOApp.Simple:

  trait CustomApplicativeError[F[_], E] extends Applicative[F]:
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  trait CustomMonadError[F[_], E] extends CustomApplicativeError[F, E] with Monad[F]

  trait CustomPoll[F[_]]:
    def apply[A](fa: F[A]): F[A]

  trait CustomMonadCancel[F[_], E] extends CustomMonadError[F, E]:
    def cancelled: F[Unit]
    def uncancellable[A](poll: Poll[F] => F[A]): F[A]

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values
  val molIO: IO[Int]          = monadCancelIO.pure(42)
  val ambitiousModIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute: IO[Int] =
    monadCancelIO.uncancelable { _ =>
      for
        _   <- monadCancelIO.pure("once started, I can't go back...")
        res <- monadCancelIO.pure(56)
      yield res
    }

  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] =
    mc.uncancelable { _ =>
      for
        _   <- mc.pure("once started, I can't go back...")
        res <- mc.pure(56)
      yield res
    }

  val mustCompute_v2: IO[Int] = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener: IO[Int]    = mustCompute.onCancel(IO("I'm being cancelled!").void)
  val mustComputeWithListener_v2: IO[Int] =
    monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled!").void)
  // onCancel as an extension method
  // import cats.effect.syntax.monadCancel.* //

  // allow finalizers
  val computationWithFinalizers: IO[Int] = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(effect) => effect.flatMap(a => IO(s"successful: $a").void)
    case Errored(e)        => IO(s"failed: $e").void
    case Canceled()        => IO("cancelled").void
  }

  // bracket pattern is specific to MonadCancel
  val computationWithUsage: IO[String] =
    monadCancelIO.bracket(IO(42)) { value =>
      IO(s"using the meaning of life: $value")
    } { value =>
      IO("releasing the meaning of life...").void
    }

  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // non-semantic blocking!!!

  // exercise - generalize a piece of code
  def inputPassword[F[_], E](using mc: MonadCancel[F, E]): F[String] =
    for
      _   <- mc.pure("Input password: ").debug
      _   <- mc.pure("(typing password)").debug
      _   <- unsafeSleep(5.seconds)
      pwd <- mc.pure("ThePassword1")
    yield pwd

  def verifyPassword[F[_], E](pw: String)(using mc: MonadCancel[F, E]): F[Boolean] =
    for
      _        <- mc.pure("verifying...").debug
      _        <- unsafeSleep(2.seconds)
      verified <- mc.pure(pw == "ThePassword!")
    yield verified

  def authFlow[F[_], E](using mc: MonadCancel[F, E]): F[Unit] =
    mc.uncancelable { poll =>
      for
        pw       <- poll(inputPassword)
                      .onCancel(
                        mc.pure("Authentication timed out. Try again later.").debug.void
                      )
        verified <- verifyPassword(pw)
        _        <- if verified then mc.pure("Authentication successful.").debug
                    else mc.pure("Authentication failed.").debug
      yield ()
    }

  val authProgram: IO[Unit] =
    for
      authFib <- authFlow[IO, Throwable].start
      _       <- IO.sleep(3.second) >> IO("Auth timed out, attempting cancel...").debug >> authFib.cancel
      _       <- authFib.join
    yield ()

  override def run: IO[Unit] = authProgram
