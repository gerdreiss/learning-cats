import cats.effect.*

object Playground extends IOApp.Simple:
  override def run: IO[Unit] =
    IO.println("Learning Cats Effect 3!")
