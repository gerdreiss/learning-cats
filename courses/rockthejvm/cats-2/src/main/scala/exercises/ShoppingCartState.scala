package exercises

import cats.Monoid
import cats.data.State
import cats.syntax.monoid.*

object ShoppingCartState extends App:

  case class ShoppingCart(items: List[String], total: BigDecimal)

  given Monoid[ShoppingCart] with
    def empty: ShoppingCart = ShoppingCart(List.empty, 0)
    def combine(cart1: ShoppingCart, cart2: ShoppingCart): ShoppingCart =
      ShoppingCart(cart1.items ++ cart2.items, cart1.total + cart2.total)

  def addToCart(item: String, price: BigDecimal): State[ShoppingCart, BigDecimal] =
    State { cart =>
      (cart |+| ShoppingCart(List(item), price), cart.total + price)
    }

  var resultState: State[ShoppingCart, BigDecimal] =
    for
      _     <- addToCart("Item1", 99.00)
      _     <- addToCart("Item2", 29.90)
      total <- addToCart("Item3", 199.90)
    yield total

  val (state, result)  = resultState.run(summon[Monoid[ShoppingCart]].empty).value
  println(state)
  println(result)
