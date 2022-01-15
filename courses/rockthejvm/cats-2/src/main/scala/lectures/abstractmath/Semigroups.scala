package lectures.abstractmath

// semigroups combine elements of same type
import cats.Semigroup
// these imports are unnecessary, for some reason...
//import cats.instances.int.*
//import cats.instances.option.*
//import cats.instances.string.*
import cats.syntax.semigroup.*

case class Expense(id: Long, amount: BigDecimal)

given expenseSemigroup: Semigroup[Expense] =
  Semigroup.instance[Expense] { (l, r) =>
    Expense(math.max(l.id, r.id), l.amount |+| r.amount)
  }

// generalized API
def reduceThings[T](ts: List[T])(using semigroup: Semigroup[T]): T =
  ts.reduce(_ |+| _)

@main def semigroups(): Unit =
  val x              = 2
  val y              = 40
  val ints           = List(x, y)
  val intSemigroup   = Semigroup[Int]
  val intCombination = intSemigroup.combine(x, y)
  println(s"combined $x and $y to $intCombination")
  println(s"combined $ints via reduce(combine) to ${ints.reduce(intSemigroup.combine)}")

  val a                 = "I love "
  val b                 = "Cats!"
  val strings           = List(a, b)
  val stringSemigroup   = Semigroup[String]
  val stringCombination = stringSemigroup.combine(a, b)
  println(s"combined $a and $b to $stringCombination")
  println(s"you could combine $a and $b via |+|, too, to ${a |+| b}")
  println(s"combined $strings via reduce(combine) to ${strings.reduce(stringSemigroup.combine)}")

  println(s"combined $ints via generalized reduceThings function to ${reduceThings(ints)}")
  println(s"combined $strings via generalized reduceThings function to ${reduceThings(strings)}")

  val intOpts = ints.map(Option(_))
  println(s"combined $intOpts via generalized reduceThings function to ${reduceThings(intOpts)}")

  val intOptsWithNone = intOpts :+ None
  println(
    s"combined $intOptsWithNone via generalized reduceThings function to ${reduceThings(intOptsWithNone)}"
  )

  val exp1 = Expense(1, BigDecimal(99.90))
  val exp2 = Expense(2, BigDecimal(20.0))
  println(s"combined $exp1 and $exp2 to ${exp1 |+| exp2}")
