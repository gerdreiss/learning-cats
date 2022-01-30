package polymorphic

import cats.*
import cats.effect.*
import cats.syntax.all.*
import cats.effect.kernel.Outcome.*

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

  val mustCompute = monadCancelIO.uncancelable { _ =>
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

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener    = mustCompute.onCancel(IO("I'm being cancelled!").void)
  val mustComputeWithListener_v2 =
    monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled!").void)
  // onCancel as an extension method
  // import cats.effect.syntax.monadCancel.* //

  // allow finalizers
  val computationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(effect) => effect.flatMap(a => IO(s"successful: $a").void)
    case Errored(e)        => IO(s"failed: $e").void
    case Canceled()        => IO("cancelled").void
  }

  // bracket pattern is specific to MonadCancel
  val computationWithUsage =
    monadCancelIO.bracket(IO(42)) { value =>
      IO(s"using the meaning of life: $value")
    } { value =>
      IO("releasing the meaning of life...").void
    }

  // exercise

  override def run = ???
