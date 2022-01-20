package lectures.aliens

import cats.Monoid
import cats.Invariant
import cats.Show
import cats.syntax.invariant.*

object InvariantFunctors extends App:

  trait Crypto[A]:
    self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](backwards: B => A, forwards: A => B): Crypto[B] =
      new:
        def encrypt(value: B): String     = self.encrypt(backwards(value))
        def decrypt(encrypted: String): B = forwards(self.decrypt(encrypted))

  def encrypt[A](value: A)(using C: Crypto[A]): String     = C.encrypt(value)
  def decrypt[A](encrypted: String)(using C: Crypto[A]): A = C.decrypt(encrypted)

  given caesarCrypto: Crypto[String] =
    new:
      def encrypt(value: String): String =
        value.map(c => (c + 2).toChar)
      def decrypt(encrypted: String): String =
        encrypted.map(c => (c - 2).toChar)

  given bigDecimalCrypto: Crypto[BigDecimal]      = caesarCrypto.imap(_.toString, BigDecimal(_))
  given maybeStringCrypto: Crypto[Option[String]] = caesarCrypto.imap(_.getOrElse(""), Option(_))

  given optionCrypto[T](using C: Crypto[T], M: Monoid[T]): Crypto[Option[T]] =
    C.imap(_.getOrElse(M.empty), Option(_))

  val encrypted  = encrypt("encrypt this")
  val descrypted = decrypt[String](encrypted)

  println(encrypted)
  println(descrypted)
  println(encrypt(BigDecimal(10000)))
  println(encrypt(Option("10000")))
  println(encrypt(Option.empty[String]))
  println(decrypt[Option[String]](encrypted))
  println(encrypt(Option(BigDecimal(10000))))

  val showOptionString: Show[Option[String]] =
    Invariant[Show].imap(Show[String])(Option(_))(_.getOrElse(""))

  val showOptionStringSyntax: Show[Option[String]] =
    Show[String].imap(Option(_))(_.getOrElse(""))
