package lectures.algebra

import cats.Monad
// for pure()
import cats.syntax.applicative.*
// for flatMap()
import cats.syntax.flatMap.*
// for map()
import cats.syntax.functor.*
// for sequence()
import cats.syntax.traverse.*

// generic API
def mkTupleContextM[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
  // ma.flatMap(a => mb.map(b => (a, b)))
  for
    a <- ma
    b <- mb
  yield (a, b)

def mkTupleUsingM[M[_], A, B](ma: M[A], mb: M[B])(using M: Monad[M]): M[(A, B)] =
  M.flatMap(ma)(a => M.map(mb)(b => (a, b)))

@main def monads(): Unit =
  val numbers = List(1, 2, 3)
  val chars = List('a', 'b', 'c')
  // Exercise 1.1
  val combinations_1_1 =
    for
      n <- numbers
      c <- chars
    yield (n, c)

  // options
  val maybeNumber = Option(2)
  val maybeChar = Option('a')
  val maybeTupleFor =
    for
      n <- maybeNumber
      c <- maybeChar
    yield (n, c)
  val maybeTupleSeq = (maybeNumber, maybeChar).sequence

  // Cats Monad
  val optionMonad = Monad[Option]
  val maybeIntMonad = optionMonad.pure(42) // Some(42)
  val maybeIntTenfold = optionMonad.flatMap(maybeIntMonad)(x => optionMonad.pure(x * 10))

  val listMonad = Monad[List]
  val numbersMonad = listMonad.pure(42) // List(42)
  val numbersMonadAppendInc = listMonad.flatMap(numbersMonad)(x => List(x, x + 1))

  println(mkTupleContextM(numbers, chars))
  println(mkTupleContextM(maybeNumber, maybeChar))
  println(mkTupleUsingM(numbers, chars))
  println(mkTupleUsingM(maybeNumber, maybeChar))

  // extension methods
  val maybeOne = 1.pure[Option]
  val listOfOne = 1.pure[List]
  val maybeOneIncr = maybeOne
    .withFilter(_ > 0)
    .map(_ + 1)
    .flatMap(incrd => (incrd + 1).pure[Option])
