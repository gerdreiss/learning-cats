package concurrency

import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.applicative.*
import utils.*

import scala.concurrent.duration.*

object CancellingIO extends IOApp.Simple:

  /**
   * cancelling IOs
   * - fib.cancel
   * - IO.race & other APIs
   * - manual cancellation
   */
  // the whole thing is cancelled coz there is one cancelled in the middle
  val chainOfEffects: IO[Int] =
    IO("waiting").debug >>
      IO.canceled >>
      IO(42).debug

  // uncancellable
  // example: online store, payment processor
  //          payment process MUST NOT be cancelled
  val specialPaymentSystem =
    (
      IO("Payment running, don't cancel").debug >>
        IO.sleep(1.second) >>
        IO("Payment completed").debug
    ).onCancel(IO("Boom!").debug.void)

  val cancellationOfPay =
    for
      fib <- specialPaymentSystem.start
      _   <- IO.sleep(500.millis) >> IO("Attempt cancellation").debug >> fib.cancel
      _   <- fib.join
    yield ()

  val atomicPayment  = IO.uncancelable(_ => specialPaymentSystem) // masking
  val atomicPayment2 = specialPaymentSystem.uncancelable          // same

  val noCancellationOfPay =
    for
      fib <- specialPaymentSystem.uncancelable.start
      _   <- IO.sleep(500.millis) >> IO("Attempt cancellation").debug >> fib.cancel
      _   <- fib.join
    yield ()

  /**
   * The uncancellable API is more complex and more general.
   * It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
   * The Poll object can be used tomark sections within the returned effect which CAN BE CANCELLED.
   */

  /**
    * Example: auth service with two parts:
    * - input password, can be cancelled, coz otherwise we might block indefinitely on user input
    * - verify password, CANNOT be cancelled once it's started
    */
  val inputPassword: IO[String] =
    IO("Input password").debug >>
      IO("(typing password)").debug >>
      IO.sleep(2.seconds) >>
      IO("ThePassword")

  val verifyPassword: String => IO[Boolean] =
    (pw: String) =>
      IO("Verifying the password...").debug >>
        IO.sleep(2.seconds) >>
        IO(pw == "ThePassword")

  val authFlow: IO[Unit] =
    IO.uncancelable { poll =>
      for
        pw       <- poll(inputPassword) // this is cancellable now
                      .onCancel(IO("Authentication timed out. Try again later").debug.void)
        verified <- verifyPassword(pw)  // this and below is NOT cancellable
        _        <- if verified then IO("Auth successful").debug
                    else IO("Auth failed").debug
      yield ()
    }

  val authProgram: IO[Unit] =
    for
      authFib <- authFlow.start
      _       <- IO.sleep(3.second) >> IO("Auth timed out, attempting cancel...").debug >> authFib.cancel
      _       <- authFib.join
    yield ()

  /**
   * Uncancellable calls are MASKS which suppress cancellation.
   * Poll calls are "gaps opened" in the uncancellable region.
   */

  // EXERCISES
  // 1
  val cancelBeforeEffect = IO.canceled >> IO.pure(42).debug
  val uncancelableEffect = IO.uncancelable(_ => cancelBeforeEffect)
  // uncancellable will eliminate all cancellables, except the polled ones, obviously

  // 2 wrapping an effect in uncallelable will cancel all cancellation within that effect
  val invincibleAuthProgram: IO[Unit] =
    for
      authFib <- authFlow.uncancelable.start
      _       <- IO.sleep(1.second) >> IO("Auth timed out, attempting cancel...").debug >> authFib.cancel
      _       <- authFib.join
    yield ()

  // 3
  def threeStepProgram: IO[Unit] =
    val sequence = IO.uncancelable { poll =>
      poll(
        "First cancellable".pure[IO].debug >>
          IO.sleep(1.second) >>
          "first cancellable end".pure[IO].debug
      ) >>
        "Uncancellable".pure[IO].debug >>
        IO.sleep(1.second) >>
        "uncancellable end".pure[IO].debug >>
        poll(
          "Second cancellable".pure[IO].debug >>
            IO.sleep(1.second) >>
            "second cancellable end".pure[IO].debug
        )
    }

    for
      fib <- sequence.start
      _   <- IO.sleep(1200.millis) >> IO("CANCEL!").debug >> fib.cancel
      _   <- fib.join
    yield ()

  override def run: IO[Unit] = threeStepProgram
