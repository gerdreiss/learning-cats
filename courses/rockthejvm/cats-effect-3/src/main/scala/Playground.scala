import cats.effect.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure
import scala.annotation.tailrec

object Playground extends IOApp.Simple:

  val nums = (1 to 10000000).toList

  def lenUnsafe[A](as: List[A]): Int =
    if as.isEmpty then 0 else 1 + lenUnsafe(as.tail)

  def lenSafe[A](as: List[A]): Int =
    @tailrec
    def rec(rest: List[A], acc: Int): Int =
      if rest.isEmpty then acc
      else rec(rest.tail, acc + 1)

    rec(as, 0)

  def lenIO[A](as: List[A]): IO[BigInt] =
    if as.isEmpty then IO.pure(0)
    else IO.defer(lenIO(as.tail).map(_ + 1))

  def lenFuture[A](as: List[A]): Future[Int] =
    if as.isEmpty then Future(0)
    else Future.delegate(lenFuture(as.tail).map(_ + 1))

  override def run: IO[Unit] =
    IO.println("Learning Cats Effect 3!") *>
      IO.unit *>
      IO.println(lenSafe(nums)) *>
      // IO.println(lenUnsafe(nums)) *>
      // IO.println(lenSafe(nums)) *>
      // lenIO(nums).map(println)
      // IO.fromFuture(IO(lenFuture(nums))).map(println)
      IO.println("End.")
