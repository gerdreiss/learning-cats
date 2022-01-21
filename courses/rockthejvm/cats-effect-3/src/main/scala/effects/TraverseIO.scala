package effects

import cats.*
import cats.effect.*
import cats.syntax.all.*
import utils.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object TraverseIO extends IOApp.Simple:

  def heavyComputation(s: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    s.split(" ").length
  }

  val workload: List[String] =
    List(
      "I quite like cats effect",
      "Scala is awesome",
      "Looking forward to some great cats effect stuff"
    )

  def clunkyFutures(): Unit =
    workload.map(heavyComputation).foreach(_.foreach(println))

  def traverseFutures(): Unit =
    workload.traverse(heavyComputation).foreach(println)

  def computeIO(s: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    s.split(" ").length
  }.debug

  val ios: List[IO[Int]]              = workload.map(computeIO)
  val singleIO: IO[List[Int]]         = workload.traverse(computeIO)
  val parallelSingleIO: IO[List[Int]] = workload.parTraverse(computeIO)

  def sequence[F[_]: Traverse, A](ioas: F[IO[A]]): IO[F[A]] =
    ioas.traverse(identity)

  def parSequence[F[_]: Traverse, A](ioas: F[IO[A]]): IO[F[A]] =
    ioas.parTraverse(identity)

  override def run: IO[Unit] =
    // parallelSingleIO.map(_.sum).debug.void
    sequence(ios).map(_.sum).debug.void
