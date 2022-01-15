package intruduction

case class Person(name: String, age: Int)

// part 1: type classes definition
trait JsonSer[T]:
  def toJson(value: T): String

// part 2: create implicit type classes instances
given JsonSer[String] with
  def toJson(value: String): String = s""""$value"""""

// part 3:

@main def typeClasses(): Unit = ???
