import cats.effect.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

object Playground extends IOApp.Simple:

  val nums = (1 to 10000000).toList

  def len[A](as: List[A]): BigInt =
    if as.isEmpty then 0 else 1 + len(as.tail)

  def lenIO[A](as: List[A]): IO[BigInt] =
    if as.isEmpty then IO.pure(0)
    else IO.defer(lenIO(as.tail).map(_ + 1))

  def lenFuture[A](as: List[A]): Future[Int] =
    if as.isEmpty then Future(0)
    else Future.delegate(lenFuture(as.tail).map(_ + 1))

  override def run: IO[Unit] =
    IO.println("Learning Cats Effect 3!") *>

    // len(nums)
    // lenIO(nums).map(println)
    IO.fromFuture(IO(lenFuture(nums))).map(println)
