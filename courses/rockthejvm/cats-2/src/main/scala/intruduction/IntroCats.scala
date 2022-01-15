package intruduction

@main def introCats(): Unit =
  // Eq
  // part 1 - type class import
  import cats.Eq
  // part 2 - type class instance import
  import cats.instances.int.*
  // part 3 - extension method import
  import cats.syntax.eq.*

  // part 4 - use the type class API
  val a = 1
  val b = 2
  println(s"is $a equal to $b (using eqv)? " + (if Eq[Int].eqv(a, b) then "yes" else "no"))
  // part 5 - use the extension methods
  println(s"is $a equal to $b (using ===)? " + (if a === b then "yes" else "no"))
  println(s"is $a not equal to $b (using =!=)? " + (if a =!= b then "yes" else "no"))

  // part 6 - extending the type class operations to composite types
  val as = List(1)
  val bs = List(b)
  println(s"is $as equal to $bs (using ===)? " + (if as === bs then "yes" else "no"))

  // part 6 - create a type class instance for an unsuported type
  case class ToyCar(model: String, price: Double)
  // given toyCarEq: Eq[ToyCar] =
  //   Eq.instance[ToyCar]((car1, car2) => car1.price === car2.price)
  given Eq[ToyCar] with
    def eqv(car1: ToyCar, car2: ToyCar): Boolean =
      car1.model === car2.model &&
        car1.price === car2.price

  val car1 = ToyCar("Matchbox", 100.0)
  var car2 = ToyCar("Toys'R'Us", 200.0)
  println(s"is $car1 equal to $car2 (using ===)? " + (if car1 === car2 then "yes" else "no"))
