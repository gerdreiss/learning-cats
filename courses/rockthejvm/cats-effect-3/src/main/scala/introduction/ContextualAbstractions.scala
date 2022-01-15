package introduction

given defaultIncrement: Int = 10

def increment(x: Int)(using inc: Int): Int   = x + inc
def multiply(x: Int)(using factor: Int): Int = x * factor

val twelve: Int  = increment(2)
val hundred: Int = multiply(10)

// implicit/given instances
trait Combiner[A]:
  def combine(x: A, y: A): A
  def empty: A

def combineAll[A](values: List[A])(using combiner: Combiner[A]): A =
  values.foldLeft(combiner.empty)(combiner.combine)

given Combiner[Int] with
  def combine(x: Int, y: Int): Int = x + y
  def empty: Int                   = 0

// synthesize given instances
given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with
  def combine(x: Option[T], y: Option[T]): Option[T] =
    (x, y) match
      case (Some(valX), Some(valY)) => Some(combiner.combine(valX, valY))
      case (Some(valX), _)          => Some(valX)
      case (_, Some(valY))          => Some(valY)
      case _                        => Some(combiner.empty)

  def empty: Option[T] = Some(combiner.empty)

// extension methods
case class Person(name: String):
  def greet: String = s"Hi, my name is $name"

extension (name: String) def greet: String = s"Hi, my name is $name"

extension [T](ts: List[T])
  def reduceThat(using combiner: Combiner[T]): T =
    ts.foldLeft(combiner.empty)(combiner.combine)

// type classes

case class Person2(name: String, age: Int)

// Part 1 : type class definition
trait JsonSer[T]:
  def toJson(value: T): String

// Part 2 : type class instances
given JsonSer[String] with
  def toJson(s: String): String = s""""$s""""

given JsonSer[Int] with
  def toJson(i: Int): String = i.toString

given JsonSer[Person2] with
  def toJson(p: Person2): String =
    s"""{
        |  "name" : "${p.name}",
        |  "age" : ${p.age}
        |}""".stripMargin

// Part 3: user-facing API
def mkJson[T](value: T)(using serializer: JsonSer[T]): String =
  serializer.toJson(value)

extension [T](ts: List[T])
  def mkJson(using serializer: JsonSer[T]): String =
    ts.map(t => serializer.toJson(t)).mkString("[", ",\n", "]")

@main def ContextualAbstractions(): Unit =
  println(combineAll(List(1, 2, 3, 5)))
  println(combineAll(List(Some(1), None, Some(3), Some(5))))
  println(Person("Joe").greet)
  println("Jane".greet)
  println(List(1, 2, 3, 5).reduceThat)
  println(List(Person2("Joe", 40), Person2("Jane", 30)).mkJson)
