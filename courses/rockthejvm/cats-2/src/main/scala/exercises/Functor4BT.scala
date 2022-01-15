package exercises

import cats.Functor
import cats.syntax.functor.*

trait Tree[+T]
case class Leaf[+T](value: T) extends Tree[T]
case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

object Tree:
  def l[T](value: T): Leaf[T] =
    Leaf(value)

  def b[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
    Branch(value, left, right)

given Functor[Tree] with
  def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
    fa match
      case Leaf(v)         => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))

// this is implemented in cats.syntax.functor.*
// extension [T](tree: Tree[T])
//   def map[S](f: T => S)(using F: Functor[Tree]): Tree[S] =
//     F.map(tree)(f)

// using context bound restriction for the generic higher kinded type
def tenfold[F[_]: Functor](fa: F[Int]): F[Int] = fa.map(_ * 10)

@main def treeFunctor(): Unit =
  val tree = Tree.b(1, Tree.b(2, Tree.l(3), Tree.l(4)), Tree.l(5))
  println(tree.map(_ * 10))
  println(tenfold(tree))
