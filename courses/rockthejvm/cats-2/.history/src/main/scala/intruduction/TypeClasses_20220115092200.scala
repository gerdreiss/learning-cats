package intruduction

case class Person(name: String, age: Int)

// part 1: type classes definition
trait JsonSer[T]:
  def toJson(value: T): String

// part 2: create implicit type classes instances
given JsonSer[String] with
  def toJson(value: String): String = s""""$value"""""

given JsonSer[Int] with
  def toJson(value: Int): String = value.toString

given personSer[Person](using stringSer: JsonSer[String], intSer: JsonSer[Int]): JsonSer[Person]
  with
  def toJson(value: Person): String =
    s"""
      """

// part 3:

@main def typeClasses(): Unit = ???
