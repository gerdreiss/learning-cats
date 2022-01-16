package exercises

import cats.Monad

import scala.annotation.tailrec

sealed trait MTree[+T]
case class MLeaf[+T](value: T) extends MTree[T]
case class MBranch[+T](left: MTree[T], right: MTree[T]) extends MTree[T]

object MTree:
  def l[T](value: T): MLeaf[T] =
    MLeaf(value)

  def b[T](left: MTree[T], right: MTree[T]): MTree[T] =
    MBranch(left, right)

given Monad[MTree] with
  def pure[A](value: A): MTree[A] = MLeaf(value)

  def flatMap[A, B](ta: MTree[A])(f: A => MTree[B]): MTree[B] =
    ta match
      case MLeaf(a)      => f(a)
      case MBranch(l, r) => MBranch(flatMap(l)(f), flatMap(r)(f))

  def tailRecM[A, B](a: A)(f: A => MTree[Either[A, B]]): MTree[B] =
    def stackRec(t: MTree[Either[A, B]]): MTree[B] =
      f(a) match
        case MLeaf(Left(a))  => stackRec(f(a))
        case MLeaf(Right(b)) => MLeaf(b)
        case MBranch(l, r)   => MBranch(stackRec(l), stackRec(r))

    @tailrec
    def tailRec(
        todo: List[MTree[Either[A, B]]],
        expanded: Set[MTree[Either[A, B]]],
        done: List[MTree[B]]
    ): MTree[B] =
      if todo.isEmpty then done.head
      else
        todo.head match
          case MLeaf(Left(a))  => tailRec(f(a) :: todo.tail, expanded, done)
          case MLeaf(Right(b)) => tailRec(todo.tail, expanded, MLeaf(b) :: done)
          case branch @ MBranch(left, right) =>
            if expanded.contains(branch) then
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = MBranch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            else tailRec(right :: left :: todo, expanded + branch, done)

    tailRec(List(f(a)), Set.empty, List.empty)

def mTreeTenfold(t: MTree[Int]): MTree[Int] =
  summon[Monad[MTree]].flatMap(t)(i => MLeaf(i * 10))

@main def treeMonad(): Unit =
  println(mTreeTenfold(MTree.b(MTree.l(1), MTree.b(MTree.l(2), MTree.l(3)))))
