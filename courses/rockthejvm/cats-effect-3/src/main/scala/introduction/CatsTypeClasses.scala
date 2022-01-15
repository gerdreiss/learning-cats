package introduction

import cats.Functor
import cats.instances.list

/**   - applicative
  *   - functor
  *   - flatMap
  *   - monad
  *   - apply
  *   - applicativeError/monadError
  *   - traverse
  */

trait Functr[F[_]]:
  def map[A, B](initialValue: F[A])(f: A => B): F[B]
