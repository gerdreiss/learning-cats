trait Solver[F[_]]:
  def solve(givens: List[Value.Given]): F[List[Value]]

case class Coord(row: Int, col: Int):
  def isPeerOf(that: Coord): Boolean =
    (inSameRowAs(that) || inSameColAs(that) || inSameBoxAs(that)) && notThis(that)

  private def notThis(that: Coord): Boolean     = this != that
  private def inSameRowAs(that: Coord): Boolean = this.row == that.row
  private def inSameColAs(that: Coord): Boolean = this.col == that.col
  private def inSameBoxAs(that: Coord): Boolean =
    (this.row / 3) == (that.row / 3) && (this.col / 3) == (that.col / 3)

object Coord:
  val rowIndices: List[Int] = (0 to 8).toList
  val colIndices: List[Int] = (0 to 8).toList

  val allCoords: List[Coord] =
    for
      row <- rowIndices
      col <- colIndices
    yield Coord(row, col)

sealed trait Value:
  val coord: Coord
  val value: Int

object Value:
  case class Given(coord: Coord, value: Int) extends Value

sealed trait Candidate:
  val coord: Coord

object Candidate:
  class Single private[Candidate] (override val coord: Coord, override val value: Int)
      extends Value
      with Candidate

  class Multiple private[Candidate] (override val coord: Coord, candidates: Set[Int])
      extends Candidate:
    def refine(peerValue: Value): Candidate =
      val newValues = candidates -- Option.when(coord.isPeerOf(peerValue.coord))(peerValue.value)
      newValues.toList match
        case Nil                    => throw IllegalStateException()
        case singleCandidate :: Nil => Single(coord, singleCandidate)
        case multipleCandidates     => Multiple(coord, multipleCandidates.toSet)

  def initial(coord: Coord): Multiple = Multiple(coord, (1 to 9).toSet)

@main def hello: Unit =
  println("Hello world!")
