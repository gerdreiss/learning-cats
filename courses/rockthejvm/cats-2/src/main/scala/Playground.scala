import cats.implicits.*
import scala.util.Try

@main def playground: Unit =
  println(false.guard[Option].as(42).liftTo[Try](new Exception("Boom!")))
