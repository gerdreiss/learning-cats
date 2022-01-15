package com.github.learningcats.typeclasses.comparable

final case class Cat(name: String, age: Int, color: String)

object Cats {
  val murzik = Cat("Murzik", 6, "fire")
  val vasya = Cat("Vasya", 8, "fire")
}
