package coordination

import cats.effect.*
import cats.syntax.parallel.*
import scala.concurrent.duration.*
import utils.*

object Refs extends IOApp.Simple:

  // ref - purely functional atomic reference
  val ioRef42: IO[Ref[IO, Int]] = Ref[IO].of(42) // IO.ref(2)

  // modifying a ref is an effect
  val refSet43: IO[Unit] = ioRef42.flatMap(_.set(43))

  val io42: IO[Int]    = ioRef42.flatMap(_.get)
  val ioSet43: IO[Int] = ioRef42.flatMap(_.getAndSet(43))

  // updating with a function
  val ioUpdateRef42: IO[Unit] = ioRef42.flatMap(_.update(_ * 2))

  val tenfoldIoRef420: IO[Int] = ioRef42.flatMap(_.updateAndGet(_ * 10))

  // modifying with a function returning a diff type
  val ioRefModified42: IO[String] =
    ioRef42.flatMap(_.modify(v => (v * 10, s"my current value is $v")))

  // why do we need an atomic reference?
  def concurrentWork: IO[Unit] =
    def task(total: Ref[IO, Int])(workload: String): IO[Unit] =
      val wordCount = workload.split(" ").length

      for
        _        <- IO(s"Count words for '$workload': $wordCount").debug
        newCount <- total.updateAndGet(_ + wordCount)
        j_       <- IO(s"New total: $newCount").debug
      yield ()

    for
      initialCount <- Ref[IO].of(0)
      _            <- List(
                        "I love Cats Effect",
                        "This ref thing is useless",
                        "Daniel writes a lot of code"
                      )
                        .map(task(initialCount))
                        .parSequence
    yield ()

  // exercises
  def tickingClockG: IO[Unit] =
    // this reference is never updated because (see below)
    val ticks: IO[Ref[IO, Int]] = Ref[IO].of(0)

    def tickingClock: IO[Unit] =
      for
        _ <- IO.sleep(1.second)
        _ <- IO(System.currentTimeMillis).debug
        _ <- ticks.flatMap(_.updateAndGet(_ + 1))
        _ <- tickingClock
      yield ()

    def printTicks: IO[Unit] =
      for
        _ <- IO.sleep(5.seconds)
        _ <- ticks
               .flatMap(_.modify(ts => (ts, s"TICKS: $ts")))
               .debug // new reference, the original is never updated
        _ <- printTicks
      yield ()

    for _ <- (tickingClock, printTicks).parTupled
    yield ()

  def tickingClockDan: IO[Unit] =
    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] =
      for
        _ <- IO.sleep(1.second)
        _ <- IO(System.currentTimeMillis).debug
        _ <- ticks.update(_ + 1) // thread safe update
        _ <- tickingClock(ticks)
      yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] =
      for
        _  <- IO.sleep(5.seconds)
        ts <- ticks.get
        _  <- IO(s"TICKS: $ts").debug // new reference, the original is never updated
        _  <- printTicks(ticks)
      yield ()

    for
      tickRef <- Ref[IO].of(0)
      _       <- (tickingClock(tickRef), printTicks(tickRef)).parTupled
    yield ()

  def tickingClockWeird: IO[Unit] =
    // this reference is never updated because (see below)
    val ticks: IO[Ref[IO, Int]] = Ref[IO].of(0)

    def tickingClock: IO[Unit] =
      for
        ts <- ticks // new reference, the original is never updated
        _  <- IO.sleep(1.second)
        _  <- IO(System.currentTimeMillis).debug
        _  <- ts.update(_ + 1)
        _  <- tickingClock
      yield ()

    def printTicks: IO[Unit] =
      for
        ts     <- ticks // new reference, the original is never updated
        _      <- IO.sleep(5.seconds)
        currTs <- ts.get
        _      <- IO(currTs, s"TICKS: $currTs").debug
        _      <- printTicks
      yield ()

    for _ <- (tickingClock, printTicks).parTupled
    yield ()

  override def run = tickingClockWeird
