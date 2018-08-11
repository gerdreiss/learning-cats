package com.github.learningcats.monoids

import cats.Monoid

object BooleanMonoids {

  implicit val booleanAndM: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val booleanOrM: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val booleanXOrM: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }
  implicit val booleanXNorM: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)
  }

}
