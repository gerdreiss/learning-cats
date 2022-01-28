package coordination

import cats.effect.*

abstract class CustomCyclicBarrier:
  def await: IO[Unit]

object CustomCyclicBarrier:

  case class State(waiting: Int, signal: Deferred[IO, Unit])

  def apply(count: Int): IO[CustomCyclicBarrier] =
    for
      signal <- Deferred[IO, Unit]
      state  <- Ref[IO].of(State(count, signal))
    yield new CustomCyclicBarrier:
      override def await: IO[Unit] =
        Deferred[IO, Unit].flatMap { newSignal =>
          state.modify {
            case State(1, signal) => State(count, newSignal) -> signal.complete(()).void
            case State(n, signal) => State(n - 1, signal)    -> signal.get
          }
        }.flatten
