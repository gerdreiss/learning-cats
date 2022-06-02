package mtl_

import cats.Applicative
import cats.Id
import cats.Monad
import cats.data.Chain
import cats.data.Writer
import cats.effect.ExitCode
import cats.effect.IO
import cats.mtl.Tell
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.mtl.syntax.tell.*

object TellF extends cats.effect.IOApp:

  type Log[A] = Writer[Chain[String], A]

  // TODO why do I have to implement a Monad myself? do I, or is an import missing?
  given Monad[Log] with
    override def flatMap[A, B](fa: Log[A])(f: A => Log[B]): Log[B] =
      fa.flatMap(f)

    override def pure[A](x: A): Log[A] =
      Writer.value(x)

    override def tailRecM[A, B](a: A)(f: A => Log[Either[A, B]]): Log[B] =
      for
        result <- f(a)
        value  <- result match
                    case Right(b) => pure(b)
                    case Left(a)  => tailRecM(a)(f)
      yield value

  case class ServiceParams(option1: String, option2: Int)
  case class ServiceResult(userId: Int, companies: List[String])

  def serviceCall[F[_]: Applicative](params: ServiceParams): F[ServiceResult] =
    ServiceResult(0, List("Raven Enterprises")).pure[F]

  def serviceCallWithLog[F[_]: Monad](
      params: ServiceParams
  )(using Tell[F, Chain[String]]): F[ServiceResult] =
    for
      _      <- Chain.one(show"Call to service with ${params.option1} and ${params.option2}").tell
      result <- serviceCall[F](params)
      _      <- Chain.one(show"Service returned: userId: ${result.userId}; companies: ${result.companies}").tell
    yield result

  def run(args: List[String]): IO[ExitCode] =
    val (log, result): (Chain[String], ServiceResult) =
      serviceCallWithLog(ServiceParams("business", 42)).run

    for
      _ <- IO(println(log))
      _ <- IO(println(result))
    yield ExitCode.Success
