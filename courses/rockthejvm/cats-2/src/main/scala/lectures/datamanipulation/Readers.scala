package lectures.datamanipulation

import cats.Id
import cats.data.Reader
import cats.syntax.flatMap.*

import scala.util.Random

object Readers extends App:

  case class Config(
      replyTo: String,
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      numThreads: Int
  )

  class DbService(username: String, password: String):
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = Random.nextLong()

  class HttpService(host: String, port: Int):
    def start(): Unit = println("server started")

  class EmailService(emailReplyTo: String):
    def sendEmail(address: String, contents: String) =
      s"From $emailReplyTo to $address >>> $contents"

  val config: Config =
    Config(
      "joe@email.com",
      "Joe",
      "pwd",
      "localhost",
      8080,
      Runtime.getRuntime.availableProcessors()
    )
  val dbReader: Reader[Config, DbService] = Reader(conf => DbService(conf.dbUsername, conf.dbPassword))
  val dbConn: Id[DbService] = dbReader.run(config)
  val emailReader: Reader[Config, EmailService] = Reader(config => EmailService(config.replyTo))
  val emailService: Id[EmailService] = emailReader.run(config)

  // Reader[I, O]
  val orderStatusReader: Reader[Config, String] = dbReader.map(dbConn => dbConn.getOrderStatus(42))
  val orderStatus = dbReader.run(config)

  def getLastOrderStatus(username: String): Reader[Config, String] =
    dbReader
      .map(dbConn => dbConn.getLastOrderId(username))
      .flatMap(orderId => dbReader.map(_.getOrderStatus(orderId)))

  def getLastOrderStatusFor(user: String): Reader[Config, String] =
    for
      orderId <- dbReader.map(_.getLastOrderId(user))
      orderStatus <- dbReader.map(_.getOrderStatus(orderId))
    yield orderStatus

  def sendStatus(address: String)(orderStatus: String): Reader[Config, Unit] =
    emailReader.map(_.sendEmail(address, s"the status of your last order is $orderStatus"))

  def emailUser(username: String, email: String): Reader[Config, Unit] =
    getLastOrderStatusFor(username) >>= sendStatus(email)

  println(getLastOrderStatus("Joe").run(config))
  println(getLastOrderStatusFor("Joe").run(config))
