import cats.data.*
import cats.effect.*
import cats.implicits.*

case class Transaction(fromAccount: String, toAccount: String, amount: Long)

sealed trait BankValidation:
  def error: String

case object InvalidAccount extends BankValidation:
  def error = s"The account number should contain 10 digits"

case object InvalidAmount extends BankValidation:
  def error = "The transfer account must be greater than 0"

object Main extends IOApp.Simple:
  def validateAccount(account: String): ValidatedNel[BankValidation, String] =
    Validated
      .cond(
        account.matches("[0-9]{10}"),
        account,
        InvalidAccount
      )
      .toValidatedNel

  def validateAmount(amount: Long): ValidatedNel[BankValidation, Long] =
    Validated
      .cond(
        amount > 0,
        amount,
        InvalidAmount
      )
      .toValidatedNel

  def validateInput(
      fromAccount: String,
      toAccount: String,
      amount: Long
  ): ValidatedNel[BankValidation, Transaction] =
    (
      validateAccount(fromAccount),
      validateAccount(toAccount),
      validateAmount(amount)
    ).mapN(Transaction.apply)

  def run: IO[Unit] =
    for
      valid <- validateInput(
                 "1234567890",
                 "1234567891",
                 100
               ).pure[IO]
      _     <- IO.println(valid)
    yield ()
