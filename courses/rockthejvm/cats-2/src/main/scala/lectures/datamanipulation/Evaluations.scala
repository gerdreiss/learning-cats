package lectures.datamanipulation

import cats.Eval

object Evaluations extends App:

  val instantEval = Eval.now {
    println("Computing now!")
    42
  }

  val redoEval = Eval.always {
    println("Computing every time!")
    43
  }

  val delayedEval = Eval.later {
    println("Computing later!")
    44
  }

  val composedEval = for
    v1 <- instantEval
    v2 <- redoEval
    v3 <- delayedEval
  yield v1 + v2 + v3

  // println(instantEval.value)
  // println(instantEval.value)
  // println(instantEval.value)
  // println(redoEval.value)
  // println(redoEval.value)
  // println(redoEval.value)
  // println(delayedEval.value)
  // println(delayedEval.value)
  // println(delayedEval.value)
  println(composedEval.value)
  println(composedEval.value)
  println(composedEval.value)

  val doNotReevaluate = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step 1...")
      "put the guitar on your lap"
    }
    .map { step1 =>
      println("Step 2...")
      s"$step1, then put your left hand on the neck"
    }
    .memoize
    .map { stepsOneAndTwo =>
      println("Step 3, more complicated")
      s"$stepsOneAndTwo, then with the right hand strike the strings"
    }

  println("-" * 50)
  println(tutorial.value)
  println(tutorial.value)
  println("-" * 50)
  println("deferring evaluation...")

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  println(defer(Eval.now {
    println("Now!")
    42
  }).value)

  def reverseList[T](ts: List[T]): List[T] =
    if ts.isEmpty then ts
    else reverseList(ts.tail) :+ ts.head

  def reverseEval[T](ts: List[T]): Eval[List[T]] =
    if ts.isEmpty then Eval.later(ts)
    else Eval.defer(reverseEval(ts.tail).map(_ :+ ts.head))

  // println(reverseList((1 to 10000).toList))
  println(reverseEval((1 to 10000).toList).value)
