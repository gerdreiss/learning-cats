package lectures.typeclasses

import cats.ApplicativeError
import cats.MonadError
import cats.data.Validated
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.monadError.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try

object HandlingErrors extends App:

  type ErrorOr[T] = Either[String, T]

  val MES = MonadError[ErrorOr, String]

  val failure: ErrorOr[Int]   = MES.raiseError[Int]("Boom!") // Either[String, Int] = Left("Boom!")
  val success: ErrorOr[Int]   = MES.pure(42)                 // Either[String, Int] = Right(42)
  val recovered: ErrorOr[Int] = MES.handleError(failure)(_ => -1)
  val handled: ErrorOr[Int]   = MES.handleErrorWith(failure)(_ => 44.pure)
  val filtered: ErrorOr[Int]  = MES.ensure(success)("Odd")(_ % 2 == 0)

  // Try and Future
  val MET = MonadError[Try, Throwable]

  val exception: Try[Int] = MET.raiseError(Exception("Boom!"))

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
  )

  val MEF = MonadError[Future, Throwable]

  val futureError: Future[Int] = MEF.raiseError(Exception("Future Boom!"))

  // applicatives => ApplicativeError

  type InvalidOr[T] = Validated[List[String], T]

  val AE = ApplicativeError[InvalidOr, List[String]]

  // extension methods

  val succ = 42.pure[InvalidOr]
  val fail = List("Booms!").raiseError[InvalidOr, Int]
  val recd = fail.recover(_ => -1)
  val sure = succ.ensure("Unsure!")(_ % 2 == 0)

  println(succ)
  println(fail)
  println(recd)
  println(sure)
