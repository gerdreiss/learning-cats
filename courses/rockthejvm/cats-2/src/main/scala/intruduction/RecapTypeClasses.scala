package intruduction

object Model:

  enum Gender:
    case MALE, FEMALE, OTHER

  case class Person(name: String, gender: Gender, age: Int)

end Model

object Json:
  import Model.*

  // part 1: type classes definition
  trait JsonSer[T]:
    def toJson(value: T): String

  // part 2: create implicit type classes instances
  given JsonSer[String] with
    def toJson(value: String): String = s""""$value"""""

  given JsonSer[Int] with
    def toJson(value: Int): String = value.toString

  given genderSer(using stringSer: JsonSer[String]): JsonSer[Gender] with
    def toJson(value: Gender): String =
      value match
        case Gender.MALE   => stringSer.toJson("male")
        case Gender.FEMALE => stringSer.toJson("female")
        case Gender.OTHER  => stringSer.toJson("other")

  given personSer(using
      stringSer: JsonSer[String],
      genderSer: JsonSer[Gender],
      intSer: JsonSer[Int]
  ): JsonSer[Person] with
    def toJson(person: Person): String =
      val nameJson   = stringSer.toJson(person.name)
      val genderJson = genderSer.toJson(person.gender)
      val ageJson    = intSer.toJson(person.age)
      s"""{ "name" : $nameJson, "gender" : $genderJson, "age" : $ageJson }"""

  given listSer[T](using jsonSer: JsonSer[T]): JsonSer[List[T]] with
    def toJson(value: List[T]): String =
      value.map(jsonSer.toJson).mkString("[", ",", "]")

end Json

object JsonSyntax:
  import Json.*

  // part 3: offer some API
  extension [T](t: T)
    def mkJson(using jsonSer: JsonSer[T]): String =
      jsonSer.toJson(t)

end JsonSyntax

@main def typeClasses(): Unit =
  import Json.given
  import JsonSyntax.*
  import Model.*

  val family =
    Person("Joe", Gender.MALE, 40) ::
      Person("Jane", Gender.FEMALE, 36) ::
      Person("Jamie", Gender.MALE, 10) ::
      Nil

  println(family.mkJson)
