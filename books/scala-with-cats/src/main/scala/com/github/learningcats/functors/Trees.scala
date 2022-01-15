package com.github.learningcats.functors

import cats.Functor
import cats.syntax.functor._

object Trees extends App {

  sealed trait Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    }
  }

  println(Tree.leaf(100).map(_ * 2))
  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))
}
