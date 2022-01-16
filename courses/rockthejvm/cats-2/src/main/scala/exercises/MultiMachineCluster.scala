package exercises

import cats.data.EitherT
import cats.syntax.traverse.*

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

given ec: ExecutionContext = ExecutionContext.fromExecutorService(
  Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
)

object MultiMachineCluster:

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match
      case Some(bandwidth) => EitherT.right(Future(bandwidth))
      case None            => EitherT.left(Future("server unreachable"))

  def canWithstandSurge(server1: String, server2: String): AsyncResponse[Boolean] =
    for
      bw1 <- getBandwidth(server1)
      bw2 <- getBandwidth(server2)
    yield bw1 + bw2 > 250

  def generateTrafficSpikeReport(server1: String, server2: String): AsyncResponse[String] =
    canWithstandSurge(server1, server2).transform {
      _.map(if _ then "Yup" else "Nope")
    }

@main def testMultiMachineCluster(): Unit =
  MultiMachineCluster
    .generateTrafficSpikeReport(
      "server1.rockthejvm.com",
      "server5.rockthejvm.com"
    )
    .value
    .onComplete {
      case Success(result) => println(result)
      case Failure(error)  => println(error)
    }
