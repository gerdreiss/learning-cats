package mtl_

import cats.Monad
import cats.data.Ior
import cats.data.NonEmptyChain
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.mtl.Chronicle
import cats.mtl.syntax.chronicle.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*

object ChronicleF extends IOApp:

  type Failures = NonEmptyChain[String]

  case class Username(value: String)
  case class Password(value: String)

  case class User(name: Username, pw: Password)

  def validateUsername[F[_]: Monad](u: String)(using Chronicle[F, Failures]): F[Username] =
    if u.isEmpty then NonEmptyChain.one("Can't be empty").confess[F, Username]
    else if u.contains(".") then
      NonEmptyChain.one("Dot in name is deprecated").dictate[F].as(Username(u))
    else Username(u).pure[F]

  def validatePassword[F[_]: Monad](p: String)(using Chronicle[F, Failures]): F[Password] =
    if p.length < 8 then NonEmptyChain.one("Password too short").confess[F, Password]
    else if p.length < 10 then
      NonEmptyChain.one("Password should be longer").dictate[F].as(Password(p))
    else Password(p).pure[F]

  def validateUser[F[_]: Monad](name: String, password: String)(using
      F: Chronicle[F, Failures]
  ): F[User] =
    (validateUsername[F](name), validatePassword[F](password)).mapN(User.apply)

  override def run(args: List[String]): IO[ExitCode] =
    val luka = validateUser[Ior[Failures, *]]("Luka", "secret")
    val john = validateUser[Ior[Failures, *]]("john.doe", "secret123")
    val jane = validateUser[Ior[Failures, *]]("jane", "reallysecurepassword")

    for
      _ <- IO.println(luka)
      _ <- IO.println(john)
      _ <- IO.println(jane)
    yield ExitCode.Success
