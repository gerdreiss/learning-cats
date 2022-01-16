package exercises

import cats.data.EitherT
import cats.syntax.traverse.*

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

object MultiMachineCluster:

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match
      case Some(bandwidth) => EitherT(Future(Right(bandwidth)))
      case None            => EitherT(Future(Left("server unreachable")))

  def canWithstandSurge(server1: String, server2: String): AsyncResponse[Boolean] =
    for
      bw1 <- getBandwidth(server1)
      bw2 <- getBandwidth(server2)
    yield bw1 + bw2 > 250

  def generateTrafficSpikeReport(server1: String, server2: String): AsyncResponse[String] =
    canWithstandSurge(server1, server2).transform {
      _.map(result => if result then "Yup" else "Nope")
    }

@main def testMultiMachineCluster(): Unit =
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
  )

  MultiMachineCluster
    .generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server3.rockthejvm.com"
    )
    .value
    .onComplete {
      case Success(result) => println(result)
      case Failure(error)  => println(error)
    }
