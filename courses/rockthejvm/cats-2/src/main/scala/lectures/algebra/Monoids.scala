package lectures.algebra

import cats.Monoid

// define a general API
def combineFold[T](ts: List[T])(using monoid: Monoid[T]): T =
  ts.foldLeft(monoid.empty)(monoid.combine)

@main def monoids(): Unit =
  val numbers = (1 to 1000).toList
  val strings = List("hello", ", ", "world", "!")

  val sumLeft      = numbers.foldLeft(0)(_ + _)
  val sumRight     = numbers.foldRight(0)(_ + _)
  val sumMonoid    = combineFold(numbers)
  val concatenated = combineFold(strings)

  println(sumLeft)
  println(sumRight)
  println(sumMonoid)
  println(concatenated)

  val phonebook1 = Map("Alice" -> 234, "Bob" -> 235, "Dan" -> 100)
  val phonebook2 = Map("Charlie" -> 189, "Dan" -> 243)

  val combinedPhonebook = combineFold(List(phonebook1, phonebook2))
  println(combinedPhonebook)
