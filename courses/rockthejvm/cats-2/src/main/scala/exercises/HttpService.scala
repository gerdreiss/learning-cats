package exercises

import cats.Monad
// for >>=
import cats.syntax.flatMap.*

type ErrorOr[T] = Either[Throwable, T]

case class Connection(host: String, port: String)

trait HttpService[M[_]]:
  def getConnection(cfg: Map[String, String]): M[Connection]
  def issueRequest(payload: String)(connection: Connection): M[String]

object OptionHttpService extends HttpService[Option]:
  import cats.syntax.option.*

  override def getConnection(cfg: Map[String, String]): Option[Connection] =
    if cfg.contains("host") && cfg.contains("port") then Some(Connection(cfg("host"), cfg("port")))
    else None

  override def issueRequest(payload: String)(connection: Connection): Option[String] =
    if payload.isEmpty then none[String]
    else s"request ($payload) has been accepted".some

end OptionHttpService

object EitherHttpService extends HttpService[ErrorOr]:
  import cats.syntax.either.*

  override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
    if cfg.contains("host") && cfg.contains("port") then
      Connection(cfg("host"), cfg("port")).asRight
    else IllegalArgumentException("connection configuration incomplete").asLeft

  override def issueRequest(payload: String)(connection: Connection): ErrorOr[String] =
    if payload.isEmpty then IllegalArgumentException("invalid payload").asLeft
    else s"request ($payload) has been accepted".asRight

end EitherHttpService

def getResponse[M[_]: Monad](
    service: HttpService[M],
    config: Map[String, String],
    payload: String
): M[String] =
  service.getConnection(config) >>= service.issueRequest(payload)

@main def runHttpService(): Unit =

  val config = Map("host" -> "localhost", "port" -> "8000")
  val payload = "some payload"

  val maybeResponse =
    OptionHttpService.getConnection(config) >>=
      OptionHttpService.issueRequest(payload)

  println(maybeResponse)

  val responseOrError =
    EitherHttpService.getConnection(config) >>=
      EitherHttpService.issueRequest(payload)

  println(responseOrError)

  println("-" * 50)
  println(getResponse(OptionHttpService, config, payload))
  println(getResponse(EitherHttpService, config, payload))

end runHttpService
