package exercises

import cats.Monoid

case class ShoppingCart(items: List[String], total: BigDecimal)

given shoppingCartMonoid(using
    stringListMonoid: Monoid[List[String]],
    bigDecimalMonoid: Monoid[BigDecimal]
): Monoid[ShoppingCart] with
  def empty: ShoppingCart =
    ShoppingCart(stringListMonoid.empty, bigDecimalMonoid.empty)

  def combine(a: ShoppingCart, b: ShoppingCart): ShoppingCart =
    ShoppingCart(
      stringListMonoid.combine(a.items, b.items),
      bigDecimalMonoid.combine(a.total, b.total)
    )

def checkout(shoppingCarts: List[ShoppingCart])(using
    M: Monoid[ShoppingCart]
): ShoppingCart =
  shoppingCarts.fold(M.empty)(M.combine)

@main def monoidExercise(): Unit =
  println(
    checkout(
      List(
        ShoppingCart(List("a"), 99.90),
        ShoppingCart(List("b", "c"), 198.80),
        ShoppingCart(List("d", "e"), 1198.80)
      )
    )
  )
