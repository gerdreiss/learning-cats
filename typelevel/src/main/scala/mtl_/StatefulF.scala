package mtl_

import cats.*
import cats.data.*
import cats.effect.*
import cats.implicits.*
import cats.mtl.Stateful

object StatefulF extends IOApp:

  type Cache = Map[String, ServiceResult]

  case class ServiceResult(id: Int, companies: List[String])

  def serviceCall[F[_]: Monad](id: String): F[ServiceResult] =
    println(s"Called service with $id")
    ServiceResult(0, List("Raven Enterprises")).pure[F]

  def cachedServiceCall[F[_]: Monad](id: String)(using F: Stateful[F, Cache]): F[ServiceResult] =
    for
      cache  <- F.get
      result <- cache.get(id) match
                  case Some(result) => result.pure[F]
                  case None         => serviceCall[F](id)
    yield result

  def serviceCallAndWriteToCache[F[_]: Monad](id: String)(using F: Stateful[F, Cache]): F[ServiceResult] =
    for
      result <- serviceCall[F](id)
      cache  <- F.get
      _      <- F.set(cache.updated(id, result))
    yield result

  def invalidate[F[_]](using F: Stateful[F, Cache]): F[Unit] = F.set(Map.empty)

  def program[F[_]: Monad](using F: Stateful[F, Cache]): F[ServiceResult] =
    for
      result1     <- cachedServiceCall[F]("ab94d2")
      result2     <- cachedServiceCall[F]("ab94d2") // This should use the cached value
      _           <- invalidate[F]
      freshResult <- cachedServiceCall[F]("ab94d2") // This should access the service again
    yield freshResult

  override def run(args: List[String]): IO[ExitCode] =
    val (result, cache) = program[State[Cache, *]].run(Map.empty).value

    IO.println(show"Result: ${result.mkString}").as(ExitCode.Success)
