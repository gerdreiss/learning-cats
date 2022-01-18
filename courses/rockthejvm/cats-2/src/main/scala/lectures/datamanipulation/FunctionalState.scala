package lectures.datamanipulation

import cats.data.State

object FunctionalState extends App:

  type FunkyState[S, A] = S => (S, A)

  val countAndSay: State[Int, String] = State(cnt => (cnt + 1, s"Counted $cnt"))

  val (eleven, counted10) = countAndSay.run(10).value

  // state = "iterative" computations
  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a += 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // functional
  val firstTransformation: State[Int, String] =
    State((s: Int) => (s + 1, s"obtained $s, added 1"))
  val secondTransformation: State[Int, String] =
    State((s: Int) => (s * 5, s"obtained $s, multiplied with 5"))

  val compositeTransformation = firstTransformation.flatMap { firstResult =>
    secondTransformation.map { secondResult =>
      (firstResult, secondResult)
    }
  }

  val forComprehendedTransformation = for
    first  <- firstTransformation
    second <- secondTransformation
  yield (first, second)

  println(compositeTransformation.run(10).value)
  println(forComprehendedTransformation.run(10).value)
