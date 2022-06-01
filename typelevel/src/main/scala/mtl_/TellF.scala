package mtl_

import cats.Applicative
import cats.Id
import cats.Monad
import cats.data.Chain
import cats.data.Writer
import cats.mtl.Tell
import cats.syntax.applicative.catsSyntaxApplicativeId
import cats.syntax.flatMap.toFlatMapOps
import cats.syntax.functor.toFunctorOps
import cats.syntax.show.showInterpolator

type Log[A] = Writer[Chain[String], A]

// TODO why do I have to implement a Monad myself?
given Monad[Log] with
  override def flatMap[A, B](fa: Log[A])(f: A => Log[B]): Log[B]       = fa.flatMap(f)
  override def pure[A](x: A): mtl_.Log[A]                              = Writer.value(x)
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
  // a fake call to some external service, replace with real implementation
  ServiceResult(0, List("Raven Enterprises")).pure[F]

def serviceCallWithLog[F[_]: Monad](
    params: ServiceParams
)(using F: Tell[F, Chain[String]]): F[ServiceResult] =
  for
    _      <- F.tell(Chain.one(show"Call to service with ${params.option1} and ${params.option2}"))
    result <- serviceCall[F](params)
    _      <- F.tell(Chain.one(show"Service returned: userId: ${result.userId}; companies: ${result.companies}"))
  yield result

@main def main(): Unit =
  val (log, result): (Chain[String], ServiceResult) =
    serviceCallWithLog(ServiceParams("business", 42)).run

  println(log.toList.mkString("\n"))
  println
  println(result)
