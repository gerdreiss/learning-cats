import cats.effect.*
import cats.implicits.*

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

trait Cell:
  def coord: Coord
  def deduceSingleCandidate(allCells: List[Cell]): IO[Value]
  protected[this] def deferredValue: Deferred[IO, Value]

  def getValue: IO[Value] = deferredValue.get

  def solve(givensMap: Map[Coord, Value.Given], allCells: List[Cell]): IO[Value] =
    (givensMap.get(coord) match
      case Some(givenValue) => IO.pure(givenValue)
      case None             => deduceSingleCandidate(allCells)
    ).flatTap(deferredValue.complete)

object Cell:
  def make(_coord: Coord): IO[Cell] =
    for _deferredValue <- Deferred[IO, Value]
    yield new Cell:
      override val coord: Coord = _coord

      override val deferredValue: Deferred[IO, Value] = _deferredValue

      override def deduceSingleCandidate(allCells: List[Cell]): IO[Candidate.Single] =
        for
          refCandidate                <- Ref.of[IO, Candidate](Candidate.initial(coord))
          peerCells                    = allCells.filter(_.coord.isPeerOf(coord))
          listOfSingleCandidateOrNever =
            peerCells.map(peerCell => refineToSingleCandidateOrNever(refCandidate, peerCell))
          singleCandidate             <- raceMany(listOfSingleCandidateOrNever)
        yield singleCandidate

      private def raceMany[T](listOfIOs: List[IO[T]]) =
        listOfIOs.reduce((a, b) => a.race(b).map(_.merge))

      private def refineToSingleCandidateOrNever(
          refCandidate: Ref[IO, Candidate],
          peerCell: Cell
      ): IO[Candidate.Single] =
        for
          peerValue       <- peerCell.getValue
          singleCandidate <- refCandidate.modify {
                               case multiple: Candidate.Multiple    =>
                                 multiple.refine(peerValue) match
                                   case single: Candidate.Single     => (single, IO.pure(single))
                                   case multiple: Candidate.Multiple => (multiple, IO.never)
                               case alreadySingle: Candidate.Single => (alreadySingle, IO.never)
                             }.flatten
        yield singleCandidate

object CatsEffectDeferredRefRaceSolver extends Solver[IO]:
  def solve(givens: List[Value.Given]): IO[List[Value]] =
    for
      allCells <- Coord.allCoords.traverse(Cell.make)
      givensMap = givens.map(g => g.coord -> g).toMap
      values   <- allCells.parTraverse(_.solve(givensMap, allCells))
    yield values

@main def hello: Unit =
  println("Hello world!")
