package lectures.algebra

import cats.Monad
// for pure()
import cats.syntax.applicative.*
// for flatMap()
import cats.syntax.flatMap.*
// for map()
import cats.syntax.functor.*
// for >>=
import cats.syntax.monad.*

type LoadingOr[T] = Either[String, T]
type ErrorOr[T] = Either[Throwable, T]

case class OrderStatus(orderId: Long, status: String)
case class OrderLocation(orderId: Long, loc: String)

def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
  Right(OrderStatus(orderId = orderId, status = "Ready to ship"))

def trackLocation(orderStatus: OrderStatus): LoadingOr[OrderLocation] =
  if orderStatus.orderId > 1000 then Left("Not available yet, refreshing data...")
  else Right(OrderLocation(orderStatus.orderId, "Amsterdam, NL"))

@main def practiseMonads(): Unit =
  val listM = Monad[List]
  val listOf1 = listM.pure(1)
  val listOfMore = listM.flatMap(listOf1)(x => List(x, x + 1))

  val eitherManually: Either[String, Int] = Right(42)
  val errorM = Monad[ErrorOr]
  val errorOrInt = errorM.pure(42) // Right(42)
  val errorOrIntIncr = errorM.flatMap(errorOrInt)(x => (x + 1).pure[ErrorOr])
  println(errorOrInt)
  println(errorOrIntIncr)

  val loadingM = Monad[LoadingOr]
  val orderId = 345L
  // val orderLoc = loadingM.flatMap(getOrderStatus(orderId))(trackLocation)
  // val orderLoc = getOrderStatus(orderId).flatMap((trackLocation)
  // val orderLoc =
  //  for
  //    status <- getOrderStatus(orderId)
  //    loc <- trackLocation(status)
  //  yield loc
  val orderLoc = getOrderStatus(orderId) >>= trackLocation
