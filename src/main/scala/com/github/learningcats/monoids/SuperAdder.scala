package com.github.learningcats.monoids

import cats.Monoid
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.option._
import cats.syntax.semigroup._

object SuperAdder extends App {

  //def add(items: List[Int]): Int =
  //  // standard way of doing it:
  //  // items.foldLeft(0)(_ + _)
  //  // cats way of doing it:
  //  items.foldLeft(Monoid[Int].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit var orderM: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0.0, 0.0)
    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A](items: List[A])(implicit M: Monoid[A]): A =
    items.foldLeft(M.empty)(_ |+| _)

  println(add(List(List(1, 2), List(3, 4))))
  println(add(List(1.some, 2.some, none, 3.some)))
  println(add(List(Order(1000.0, 100), Order(2300.0, 120.0))))
}
