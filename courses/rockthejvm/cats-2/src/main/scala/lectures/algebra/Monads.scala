package lectures.algebra

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

// generic API
def mkTupleM[M[_]: Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] =
  for
    a <- fa
    b <- fb
  yield (a, b)

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

  println(mkTupleM(numbers, chars))
  println(mkTupleM(maybeNumber, maybeChar))
