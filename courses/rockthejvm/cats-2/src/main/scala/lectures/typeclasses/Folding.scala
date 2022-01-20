package lectures.typeclasses

import cats.Eval
import cats.Foldable
import cats.kernel.Monoid
import cats.syntax.foldable.*

object Folding extends App:

  // methods implemented in term of fold
  object FoldedList:
    def map[A, B](as: List[A])(f: A => B): List[B] =
      // my solution
      // as.foldLeft(List.empty)((acc, a) => acc :+ f(a))
      // Daniel's solution (I like this one better)
      as.foldRight(List.empty)((a, acc) => f(a) :: acc)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      // my solution
      // as.foldLeft(List.empty)((acc, a) => acc ++ f(a))
      // Daniel's solution, using foldRight on acc instead of ++
      as.foldLeft(List.empty)((acc, a) => acc.foldRight(f(a))(_ :: _))

    def filter[A](as: List[A])(p: A => Boolean): List[A] =
      // my solution
      // as.foldLeft(List.empty)((acc, a) => if p(a) then acc :+ a else acc)
      // Daniel's solution
      as.foldRight(List.empty)((a, acc) => if p(a) then a :: acc else acc)

    def combineAll[A](as: List[A])(using M: Monoid[A]): A =
      as.foldLeft(M.empty)(M.combine)

  end FoldedList

  val nums: List[Int] = List(1, 2, 3, 4)

  println("Folded list")
  println(FoldedList.map(nums)(_ + 1))
  println(FoldedList.flatMap(nums)(n => List(n, n * 10)))
  println(FoldedList.filter(nums)(_ > 1))
  println(FoldedList.combineAll(nums))

  println(Foldable[List].foldLeft(nums, 0)(_ + _))              // 10
  println(Foldable[Option].foldLeft(nums.headOption, 1)(_ + _)) // Some(2)
  // foldRight is stack-safe regardless of your container (size)
  println(Foldable[List].foldRight(nums, Eval.now(0))((n, eval) => eval.map(_ + n)))
  println(Foldable[List].combineAll(nums))
  println(Foldable[List].foldMap(nums)(_ * 10))

  // this is awesome!
  val nestedNums = List(
    Vector(
      Option(1),
      Option(2),
      Option(3)
    ),
    Vector(
      Option(4),
      Option(5),
      Option(6)
    )
  )
  val composedF = Foldable[List] compose Foldable[Vector] compose Foldable[Option]
  println(composedF.combineAll(nestedNums))

  // using extension methods of foldable

  println(nums.combineAll)
  println(nums.foldMap(_.toString))
