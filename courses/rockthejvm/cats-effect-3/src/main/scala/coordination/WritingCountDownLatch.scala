package coordination

import cats.effect.IOApp

import cats.effect.*

object WritingCountDownLatch extends IOApp.Simple:

  abstract class CBL:
    def await: IO[Unit]
    def release: IO[Unit]

  object CBL:
    enum State:
      case Done
      case Live(remainingCount: Int, signal: Deferred[IO, Unit])

    def apply(count: Int): IO[CBL] =
      for
        signal <- Deferred[IO, Unit]
        state  <- Ref[IO].of[State](State.Live(count, signal))
      yield new CBL:
        override def await: IO[Unit]   = state.get.flatMap { s =>
          if s == State.Done then IO.unit // continue, the latch is dead
          else signal.get
        }
        override def release: IO[Unit] = state.modify {
          case State.Live(1, signal) => State.Done                -> signal.complete(()).void
          case State.Live(n, signal) => State.Live(n - 1, signal) -> IO.unit
          case State.Done            => State.Done                -> IO.unit
        }.flatten

  override def run = ???
