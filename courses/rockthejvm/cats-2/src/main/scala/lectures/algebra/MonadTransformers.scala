package lectures.algebra

import cats.data.{ EitherT, OptionT }

import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future }

def sumAllOptions(values: List[Option[Int]]): Int =
  values.foldLeft(0) {
    case (acc, Some(value)) => acc + value
    case (acc, None)        => acc
  }

@main def monadTransformers(): Unit =
  val intOptions = List(Some(1), None, Some(3))
  val charOptions = List(Some('a'), Some('b'), None)

  println(sumAllOptions(intOptions))
  println(sumAllOptions(intOptions))

  // option transformer
  val listOfNumberOptions: OptionT[List, Int] = OptionT(intOptions)
  val listOfCharOptions: OptionT[List, Char] = OptionT(charOptions)
  val listOfTuples: OptionT[List, (Int, Char)] =
    for
      i <- listOfNumberOptions
      c <- listOfCharOptions
    yield (i, c)

  println(listOfTuples.value)

  // either transformer

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
  )

  val intEithers: List[Either[String, Int]] = List(Right(1), Left("meh"), Right(2))
  val listOfEithers: EitherT[List, String, Int] = EitherT(intEithers)
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Right(42)))
