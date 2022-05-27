package mtl_

import cats.*
import cats.data.*
import cats.implicits.*
import cats.mtl.Tell

case class ServiceParams(option1: String, option2: Int)

case class ServiceResult(userId: Int, companies: List[String])

def serviceCall[F[_]: Monad](params: ServiceParams): F[ServiceResult] =
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

// compilation fails with:
// 'Could not find an instance of Monad for cats.data.WriterT[cats.Id, cats.data.Chain[String], ?]'
// val (log, result): (Chain[String], ServiceResult) =
//   serviceCallWithLog[Writer[Chain[String], ?]](ServiceParams("business", 42)).run

@main def main(): Unit =
  // println(log)
  // println(result)
  println
