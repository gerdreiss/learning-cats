package com.github.learningcats.monads

import cats.Eval

object EvalM extends App {

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(1)
    // w/o Eval.defer this would end in StackOverflowError when using sufficiently high n
    // w Eval.defer this still could end in OutOfMemoryError when using sufficiently high n
    else Eval.defer {
      factorial(n - 1) map (_ * n)
    }

  println(factorial(50000).value)
}
