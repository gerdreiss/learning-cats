package lectures.algebra

import cats.Functor

import scala.util.Try

// generalizing API
def tenfoldList(ints: List[Int]): List[Int]                = ints.map(_ * 10)
def tenfoldOption(maybeInt: Option[Int]): Option[Int]      = maybeInt.map(_ * 10)
def tenfoldTry(evtlInt: Try[Int]): Try[Int]                = evtlInt.map(_ * 10)
def tenfold[F[_]](fa: F[Int])(using F: Functor[F]): F[Int] = F.map(fa)(_ * 10)

@main def functors(): Unit =
  val mappedList   = List(1, 2, 3).map(_ / 10.0)
  val mappedOption = Option(2).map(_ / 10.0)
  val mappedTry    = Try(42).map(_ / 10.0)

  val listFunctor   = Functor[List]
  val optionFunctor = Functor[Option]
  val tryFunctor    = Functor[Try]

  val mappedListViaFunctor   = listFunctor.map(List(1, 2, 3))(_ / 10.0)
  val mappedOptionViaFunctor = optionFunctor.map(Option(2))(_ / 10.0)
  val mappedTryViaFunctor    = tryFunctor.map(Try(42))(_ / 10.0)

  println(tenfold(List(1, 2, 3)))
  println(tenfold(Option(1)))
  println(tenfold(Try(1)))
