package coordination

import cats.effect.*
import cats.syntax.parallel.*
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

  val tenfoldIoRef42: IO[Int] = ioRef42.flatMap(_.updateAndGet(_ * 10))

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

  override def run = concurrentWork
