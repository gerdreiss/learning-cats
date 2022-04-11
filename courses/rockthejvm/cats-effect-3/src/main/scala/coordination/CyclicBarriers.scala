package coordination

import cats.effect.*
import cats.effect.std.*
import cats.syntax.parallel.*

import utils.*

import scala.concurrent.duration.*

object CyclicBarriers extends IOApp.Simple:

  /**
   * A cyclic barrier is a coordination primitive that
   * - is initialized with a count
   * - has a single API: await
   * 
   * A cyclic barrier will (semantically) block all fibers calling its await() emthod
   * until we have exactly N fibers waiting, at which point the barrier will unblock all fibers
   * and reset to its original state.
   * Any further fiber will again block until we have exactly N fibers waiting.
   * 
   * ...
   * 
   * And so on.
  */
  def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] =
    for
      _ <- IO.sleep((scala.util.Random.nextDouble * 500).toInt.millis)
      _ <- IO(s"[user $id] just heard there's this new thing - signing up... ").debug
      _ <- IO.sleep((scala.util.Random.nextDouble * 1500).toInt.millis)
      _ <- IO(s"[user $id] on the waiting list now... ").debug
      _ <- barrier.await // block the fiber when there are exactly N users waiting
      _ <- IO(s"[user $id] I'm in... ").debug
    yield ()

  def openNetwork: IO[Unit] =
    for
      _       <- IO("[announcer] network is up for registration").debug
      barrier <- CyclicBarrier[IO](10)
      _       <- (1 to 20).toList.parTraverse(id => createUser(id, barrier))
    yield ()

  override def run = openNetwork
