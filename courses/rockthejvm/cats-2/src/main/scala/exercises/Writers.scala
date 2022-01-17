package exercises

import cats.data.Writer

import scala.annotation.tailrec

object Writers extends App:

  def countAndSay(n: Int): Unit =
    if n <= 0 then println("starting...")
    else
      countAndSay(n - 1)
      println(n)

  def countAndLog(n: Int): Writer[Vector[String], Int] =
    @tailrec
    def helper(writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
      if writer.value <= 0 then writer.mapWritten(_ :+ "finished.")
      else
        val newValue = writer.value - 1
        helper(writer.bimap(_ :+ s"decreasing value to $newValue...", _ => newValue))

    helper(Writer(Vector.empty, n))

  countAndSay(10)

  println("-" * 50)
  val (logs, value) = countAndLog(10).run
  println(logs.mkString("\n"))
  println(value)
  println("-" * 50)

  def countAndLogByDaniel(n: Int): Writer[Vector[String], Int] =
    if n <= 0 then Writer(Vector("starting..."), 0)
    else countAndLogByDaniel(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  def countAndLogByDanielImproved(n: Int): Writer[Vector[String], Int] =
    if n <= 0 then Writer(Vector("starting..."), 0)
    else countAndLogByDaniel(n - 1).bimap(_ :+ s"$n", _ - 1)

  countAndLogByDaniel(10).written.foreach(println)
  println("-" * 50)
  countAndLogByDanielImproved(10).written.foreach(println)

  println("-" * 50)
  println("Naive sum:")

  def naiveSum(n: Int): Int =
    if n <= 0 then 0
    else
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n

  println(naiveSum(10))

  println("-" * 50)
  println("Logged sum:")

  def loggedSum(n: Int): Writer[Vector[String], Int] =
    if n <= 0 then Writer(Vector("starting..."), 0)
    else
      for
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- loggedSum(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      yield lowerSum + n

  loggedSum(10).written.foreach(println)
