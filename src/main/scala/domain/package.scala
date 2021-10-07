import domain.Constants._

package object domain {
  case class Coordinate(x: Int, y: Int) {
    def forward(implicit team: Team): Coordinate = copy(y = y + Team.perspective(team))
    def backward(implicit team: Team): Coordinate = copy(y = y - Team.perspective(team))
    def right(implicit team: Team): Coordinate = copy(x = x + Team.perspective(team))
    def left(implicit team: Team): Coordinate = copy(x = x - Team.perspective(team))
  }

  case class Move(from: Coordinate, to: Coordinate)

  sealed trait Team

  case class Player(team: Team)

  object Team {

    case object White extends Team
    case object Black extends Team

    def enemy(team: Team): Team =
      team match {
        case White => Black
        case Black => White
      }

    def perspective(team: Team): Int = team match {
      case Team.White => 1
      case Team.Black => -1
    }
  }

  sealed trait Piece
  object Piece {
    case object King extends Piece
    case object Queen extends Piece
    case object Bishop extends Piece
    case object Knight extends Piece
    case object Rook extends Piece
    case object Pawn extends Piece

    def isKing(p: Piece): Boolean = p match {
      case King => true
      case _ => false
    }
  }

  case class Square(coordinate: Coordinate, team: Team, piece: Piece)

  case class Board(pieces: Map[Coordinate, Square])

  object Board {
    import Piece._

    val initialState: Seq[Square] =
      fillRowWith(tableUpperBound, Team.Black , Seq(Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook)) ++
      fillRow(tableUpperBound + Team.perspective(Team.Black), Team.Black, Pawn) ++
      fillRowWith(tableLowerBound, Team.White , Seq(Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook)) ++
      fillRow(tableLowerBound + Team.perspective(Team.White), Team.White, Pawn)

    def startNew(): Board = Board(pieces = initialState.map(s => s.coordinate -> s).toMap)

    private def fillRow(y: Int, team: Team, piece: Piece): Seq[Square] = {
      fillRowWith(y, team, Seq.fill(tableUpperBound)(piece))
    }

    private def fillRowWith(y: Int, team: Team, pieces: Seq[Piece]): Seq[Square] = {
      pieces.zipWithIndex.map { case (piece, x) =>
        Square(Coordinate(x + 1, y), team, piece)
      }
    }
  }
}
