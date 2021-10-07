package domain

object Fixtures {

  object dataset {
    val WhitePawn: Square = Square(Coordinate(1,2), Team.White, Piece.Pawn)
    val BlackPawn: Square = Square(Coordinate(1,7), Team.Black, Piece.Pawn)
    val WhiteKing: Square = Square(Coordinate(2,4), Team.White, Piece.King)
    val BlackKing: Square = Square(Coordinate(7,4), Team.Black, Piece.King)
    val WhiteLeftRook: Square = Square(Coordinate(1,1), Team.White, Piece.Rook)
    val WhiteRightRook: Square = Square(Coordinate(8,1), Team.White, Piece.Rook)
    val BlackLeftRook: Square = Square(Coordinate(1,8), Team.Black, Piece.Rook)
    val BlackRightRook: Square = Square(Coordinate(8,8), Team.Black, Piece.Rook)

    def map(squares: Square*): Map[Coordinate, Square] = squares.map(s => s.coordinate -> s).toMap
  }

  import dataset._

  object scenarios {
    object base {
      val BoardWithKings: Board = Board(map(WhiteKing, BlackKing))
    }

    object empty {
      val EmptyBoard: Board = Board(Map.empty)
    }

    object single{
      val OnePawn: Board = Board(base.BoardWithKings.pieces ++ map(WhitePawn))
    }

    object friends {
      implicit val team: Team = Team.White
      val TwoFriendRooks: Board = Board(base.BoardWithKings.pieces ++ map(WhiteLeftRook, WhiteRightRook))
    }

    object enemies {
      val TwoEnemyRooks: Board = Board(base.BoardWithKings.pieces ++ map(WhiteLeftRook, BlackRightRook))
    }

    object trajectory {
      implicit val team: Team = Team.White
      val leftRook: Square = WhiteLeftRook
      val rightRook: Square = WhiteRightRook.copy(coordinate = WhiteLeftRook.coordinate.right)
      val TwoFriendRooksNextToEachOther: Board = Board(base.BoardWithKings.pieces ++ map(
        leftRook,
        rightRook
      ))
    }
  }
}
