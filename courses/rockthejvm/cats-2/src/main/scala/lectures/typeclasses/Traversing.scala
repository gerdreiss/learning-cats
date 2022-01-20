package lectures.typeclasses

import cats.Applicative
import cats.Monad
import cats.Traverse
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object Traversing extends App:

  given ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
  )

  val servers: List[String] = List(
    "server-ci.rockthejvm.com",
    "server-staging.rockthejvm.com",
    "server-prod.rockthejvm.com"
  )

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidthsFolded: Future[List[Int]] =
    servers.foldLeft(Future(List.empty[Int])) { (acc, host) =>
      for
        bs <- acc
        b  <- getBandwidth(host)
      yield bs :+ b
    }

  // val allBandwidthsFutureSequenced = Future.sequence(servers map getBandwidth)
  // val allBandwidthsFutureTraversed = Future.traverse(servers)(getBandwidth)
  // val allBandwidthsTraversed       = servers traverse getBandwidth

  def listTraverseContextBoundMonad[F[_]: Monad, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(List.empty[B].pure[F]) { (facc, a) =>
      for
        acc <- facc
        b   <- f(a)
      yield acc :+ b
    }

  def listTraverseContextBoundApplicative[F[_]: Applicative, A, B](
      as: List[A]
  )(f: A => F[B]): F[List[B]] =
    as.foldLeft(List.empty[B].pure[F]) { (facc, a) =>
      (facc, f(a)).mapN(_ :+ _)
    }

  def listSequenceContextBoundApplicative[F[_]: Applicative, A](fas: List[F[A]]): F[List[A]] =
    // listTraverseContextBoundApplicative(fas)(identity)
    fas.foldRight(List.empty[A].pure[F])((_, _).mapN(_ :: _))

  println(listSequenceContextBoundApplicative(List(Option(1), Option(2), Option(3))))
