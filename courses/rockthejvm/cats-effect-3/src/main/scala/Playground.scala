import cats.effect.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Playground extends IOApp.Simple:

  val nums = (1 to 1000000).toList

  def len[A](as: List[A]): Int =
    if as.isEmpty then 0 else 1 + len(as.tail)

  def lenFuture[A](as: List[A]): Future[Int] =
    if as.isEmpty then Future(0)
    else
      for
        one  <- Future(1)
        rest <- Future.delegate(lenFuture(as.tail))
      yield one + rest

  def lenIO[A](as: List[A]): IO[Int] =
    if as.isEmpty then IO.pure(0)
    else
      for
        one  <- IO.pure(1)
        rest <- IO.defer(lenIO(as.tail))
      yield one + rest

  override def run: IO[Unit] =
    IO.println("Learning Cats Effect 3!")

    lenIO(nums).map(println)
    IO.fromFuture(IO(lenFuture(nums))).map(println)
