package exercises

import cats.data.State

object MentalGymnastics extends App:

  // returns a State data structure that, when run,
  // will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))

  // returns a State data structure that, when run,
  // returns the value of that state and makes not changes
  def get[A]: State[A, A] = State(a => (a, a))

  // returns a State data structure that, when run,
  // returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  // returns a State data structure that, when run,
  // will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

  val program: State[Int, (Int, Int, Int)] = for
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 40)
    c <- inspect[Int, Int](_ * 2)
  yield (a, b, c)

  println(program.run(10).value)
