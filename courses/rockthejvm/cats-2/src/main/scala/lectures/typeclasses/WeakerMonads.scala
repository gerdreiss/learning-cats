package lectures.typeclasses

import cats.FlatMap
import cats.syntax.functor.*
import cats.syntax.flatMap.*

object WeakerMonads extends App:

  def ap[F[_], A, B](ff: F[A => B])(fa: F[A])(using M: FlatMap[F]): F[B] =
    M.flatMap(fa)(a => M.map(ff)(f => f(a)))

  def getPairs[F[_]: FlatMap, A, B](numbers: F[A], chars: F[B]): F[(A, B)] =
    for
      n <- numbers
      c <- chars
    yield (n, c)
