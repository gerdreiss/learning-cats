package lectures.introduction

// all these imports are seemingly unnecessary ¯\_(ツ)_/¯
// import cats.Eq
// import cats.instances.int.*
// import cats.instances.option.*
import cats.syntax.eq.*

@main def typeClassVariance(): Unit =
  val equals = Option(2) === Option(2)
  println(s"comparison result = $equals")

  // this would not work in Scala 2! O_o
  val nequals = Option(2) === None
  println(s"another comparison result = $nequals")

  // variance
  class Animal
  class Cat extends Animal
  class Dog extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated backwards to the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant (e.g. List, Option), "ACTS on T" = contravariant
  // variance affect how type class instances are being fetched

  trait SoundMaker[-T]
  given SoundMaker[Animal] with {}

  def makeSound[T](using soundMaker: SoundMaker[T]): Unit = println("wow")
  makeSound[Animal] // ok
  // compiler finds the given SoundMaker[Animal] for Cat, who is also an Animal
  makeSound[Cat]    // ok, too

  // has implication for subtypes
  given SoundMaker[Option[Animal]] with {}
  makeSound[Option[Animal]]
  makeSound[Some[Cat]]

  // covariant type class
  trait AnimalShow[+T]:
    def show: String

  given AnimalShow[Animal] with
    def show: String = "animals, animals everywhere!"

  given AnimalShow[Cat] with
    def show: String = "cats, cats everywhere!"

  def organizeShow[T](using event: AnimalShow[T]): String =
    event.show

  //  "cats, cats everywhere!" is printed
  println(organizeShow[Cat])
  // "cats, cats everywhere!" is printed here, too, because the most specific type class instance is chosen
  // this would not compile in Scala 2! O_o
  println(organizeShow[Animal])
