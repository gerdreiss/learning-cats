package effects

import cats.effect.*
import cats.effect.unsafe.implicits.global

object ExerciseIO extends App:

  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    // ioa.flatMap(_ => iob)
    // ioa >> iob // by name call -> lazy
    ioa *> iob // by value call

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    // ioa.flatMap(a => iob.map(_ => a))
    ioa <* iob

  def forever[A](ioa: IO[A]): IO[A] =
    // ioa.flatMap(_ => forever(ioa))
    // ioa *> forever(ioa) // leads to stack overflow because eager evaluation
    // ioa >> forever(ioa) // stack safe
    ioa.foreverM // cats' way of doing it

  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    // ioa.map(_ => value)
    ioa.as(value) // cats' way of doing it

  def asUnit[A](ioa: IO[A]): IO[Unit] =
    // ioa.map(_ => ())
    // ioa.as(()) // don't do this - unreadable
    ioa.void // <- that's the way

  def sum(n: Int): Int =
    if n <= 0 then 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if n == 0 then IO(0)
    else
      for
        x <- IO(n)
        y <- sumIO(n - 1)
      yield x + y

  def fib(n: Int): IO[BigInt] =
    n match
      case 0 | 1 => IO(1)
      case _     =>
        for
          a <- IO.defer(fib(n - 1)) // to make this delayed
          b <- IO.defer(fib(n - 2)) // to make this delayed
        yield a + b

  // println(sum(1000000)) // this crashes
  println(sumIO(1000000).unsafeRunSync())
  println(fib(50).unsafeRunSync())
