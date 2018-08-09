package cats.typeclasses.printable

object PrintApp extends App {

  final case class Cat(name: String, age: Int, color: String)

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val stringPrintable = new Printable[String] {
      override def format(a: String): String = a
    }
    implicit val intPrintable = new Printable[Int] {
      override def format(a: Int): String = a.toString
    }
    implicit val catPrintable = new Printable[Cat] {
      override def format(a: Cat): String = s"${a.name} is a ${a.age} year-old ${a.color} cat."
    }
  }

  object Printable {
    def format[A](value: A)(implicit ev: Printable[A]): String = ev.format(value)
    def print[A](value: A)(implicit ev: Printable[A]): Unit = println(ev.format(value))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit w: Printable[A]): String =
        w.format(value)
      def print(implicit w: Printable[A]): Unit =
        println(w.format(value))
    }
  }

  import PrintableInstances._
  import PrintableSyntax._

  Printable.print(1)
  Printable.print("hello")

  private val murzik = Cat("Murzik", 10, "fire")

  Printable.print(murzik)
  Printable.print(murzik.format)
  murzik.print
}
