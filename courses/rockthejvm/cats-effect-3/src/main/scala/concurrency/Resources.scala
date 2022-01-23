package concurrency

import cats.effect.IOApp
import cats.effect.IO
import utils.*
import scala.concurrent.duration.*
import cats.syntax.flatMap.*
import java.util.Scanner
import java.io.FileReader
import java.io.File
import cats.effect.kernel.Resource

object Resources extends IOApp.Simple:

  // use-case: manage connection lifecycle
  class Conn(val url: String):
    def open(): IO[String]  = IO(s"Opening connection to $url").debug
    def close(): IO[String] = IO(s"Closing to connection to $url").debug

  val LeakyAsyncFetchUrl =
    for
      // this connection might get leaked after cancelling this fiber
      fib <- (Conn("rockthejvm.com").open() *> IO.sleep(Int.MaxValue.seconds)).start
      _   <- IO.sleep(1.second) *> fib.cancel
    yield ()

  val correctAsyncFetchUrl =
    for
      conn <- IO(Conn("rockthejvm.com"))
      // this connection might get leaked after cancelling this fiber
      fib  <- (conn.open() *> IO.sleep(Int.MaxValue.seconds).onCancel(conn.close().void)).start
      _    <- IO.sleep(1.second) *> fib.cancel
    yield ()

  // bracket pattern
  val bracketFetchUrl = IO(Conn("rockthejvm.com"))
    .bracket(_.open() *> IO.sleep(Int.MaxValue.seconds))(_.close().void)

  val brackettedProgram = for
    fib <- bracketFetchUrl.start
    _   <- IO.sleep(1.second) *> fib.cancel
  yield ()

  def bracketReadFileG(path: String): IO[Unit] =
    IO(s"opening file $path").debug *>
      IO(Scanner(io.Source.fromFile(path).reader)).bracket { scanner =>
        def printNextLine: IO[Unit] =
          if scanner.hasNextLine then
            IO.println(scanner.nextLine) *> IO.sleep(100.millis) *> printNextLine
          else IO.unit

        IO(s"using opened file").debug *> printNextLine
      } { scanner =>
        IO(s"closing file $path").debug *> IO(scanner.close)
      }

  def bracketReadFileDan(path: String): IO[Unit] = bracketReadFileG(path)

  /**** 
    * Now to the actual Resource usage
    */

  def connectionFromConfig(path: String): IO[Unit] =
    IO(Scanner(io.Source.fromFile(path).reader))
      .bracket { scanner =>
        // acquire a connection
        IO(Conn(scanner.nextLine))
          .bracket { conn =>
            conn.open() *> IO.never
          }(conn => conn.close().void)
      }(scanner => IO(scanner.close))

  val connectionResource = Resource.make(IO(Conn("localhost")))(_.close().void)

  // ... at a later part in code
  val resourceFetchUrl = for
    fib <- connectionResource.use(conn => conn.open() *> IO.never).start
    _   <- IO.sleep(1.second) *> fib.cancel
  yield ()

  // resources are equivalent to brackets
  val simpleResource: IO[String]          = IO("some resource")
  val usingResource: String => IO[String] = s => IO(s"using the string: $s").debug
  val releaseResource: String => IO[Unit] = s => IO(s"finalizing the string: $s").debug.void

  val usingResourceWithBracket  = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  def readFileViaResource(path: String): IO[Unit] =
    IO(s"opening file $path using resource").debug *>
      Resource
        .make(IO(Scanner(FileReader(File(path))))) { scanner =>
          IO(s"close file").debug *> IO(scanner.close)
        }
        .use { scanner =>
          def printNextLine: IO[Unit] =
            if scanner.hasNextLine then
              IO.println(scanner.nextLine) *> IO.sleep(100.millis) *> printNextLine
            else IO.unit

          IO(s"using opened file").debug *> printNextLine
        }

  // nested resources
  def connectionFromConfigurationResource(config: String) =
    Resource
      .make(IO(Scanner(FileReader(File(config))))) { scanner =>
        IO(s"close file").debug *> IO(scanner.close)
      }
      .flatMap { scanner =>
        Resource.make(IO(Conn(scanner.nextLine)))(_.close().void)
      }
      .use { conn =>
        conn.open() *> IO.never
      }

  def cancelReadFile(path: String) = for
    fib <- connectionFromConfigurationResource(path).start
    _   <- IO.sleep(2.seconds) >> fib.cancel
  yield ()

  override def run = cancelReadFile("src/main/scala/concurrency/Resources.scala")
