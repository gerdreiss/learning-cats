package exercises

import cats.Functor
import cats.syntax.functor.*

sealed trait FTree[+T]
case class FLeaf[+T](value: T) extends FTree[T]
case class FBranch[+T](value: T, left: FTree[T], right: FTree[T]) extends FTree[T]

object FTree:
  def l[T](value: T): FLeaf[T] =
    FLeaf(value)

  def b[T](value: T, left: FTree[T], right: FTree[T]): FTree[T] =
    FBranch(value, left, right)

given Functor[FTree] with
  def map[A, B](fa: FTree[A])(f: A => B): FTree[B] =
    fa match
      case FLeaf(v)         => FLeaf(f(v))
      case FBranch(v, l, r) => FBranch(f(v), map(l)(f), map(r)(f))

// this is implemented in cats.syntax.functor.*
// extension [T](tree: Tree[T])
//   def map[S](f: T => S)(using F: Functor[Tree]): Tree[S] =
//     F.map(tree)(f)

// using context bound restriction for the generic higher kinded type
def tenfold[F[_]: Functor](fa: F[Int]): F[Int] = fa.map(_ * 10)

@main def treeFunctor(): Unit =
  val tree = FTree.b(1, FTree.b(2, FTree.l(3), FTree.l(4)), FTree.l(5))
  println(tree.map(_ * 10))
  println(tenfold(tree))
