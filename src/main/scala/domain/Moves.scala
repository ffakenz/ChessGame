package domain

import Constants._

sealed trait Moves {
  def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]]
}

object Moves {
  def getMoves(piece: Piece): Moves =
    piece match {
      case Piece.King => KingMoves
      case Piece.Queen => QueenMoves
      case Piece.Bishop => BishopMoves
      case Piece.Knight => KnightMoves
      case Piece.Rook => RookMoves
      case Piece.Pawn => PawnMoves
    }

  object KingMoves extends Moves {
    override def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]] =
      Seq(
        Vector apply center.left,
        Vector apply center.right,
        Vector apply center.forward,
        Vector apply center.backward,
        Vector apply center.left.backward,
        Vector apply center.right.forward,
        Vector apply center.left.backward,
        Vector apply center.right.backward,
      ).map { vector =>
        vector.filter(BoardOps.existsInBoard)
      }
  }

  object QueenMoves extends Moves {
    override def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]] =
      Seq(
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.forward),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.backward),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.right),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.left),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.forward.left),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.forward.right),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.backward.left),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.backward.right),
      ).map(_.tail.toVector).map { vector =>
          vector.filter(BoardOps.existsInBoard)
      }
  }

  object BishopMoves extends Moves {
    override def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]] =
      Seq(
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.forward.left),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.forward.right),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.backward.left),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.backward.right),
      ).map(_.tail.toVector).map { vector =>
          vector.filter(BoardOps.existsInBoard)
      }
  }

  object KnightMoves extends Moves {
    override def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]] =
      Seq(
        Vector(center.forward.forward.right),
        Vector(center.forward.forward.left),
        Vector(center.backward.backward.left),
        Vector(center.backward.backward.left),
        Vector(center.left.left.forward),
        Vector(center.left.left.backward),
        Vector(center.right.right.forward),
        Vector(center.right.right.backward)
      ).map { vector =>
        vector.filter(BoardOps.existsInBoard)
      }
  }

  object RookMoves extends Moves {
    override def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]] =
      Seq(
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.forward),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.backward),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.right),
        (tableLowerBound to tableUpperBound).foldLeft(Seq(center))((trayectory, _) => trayectory :+ trayectory.last.left)
      ).map(_.tail.toVector).map { vector =>
        vector.filter(BoardOps.existsInBoard)
      }
  }

  object PawnMoves extends Moves {
    override def getMoves(center: Coordinate, board: Board)(implicit t: Team): Seq[Vector[Coordinate]] =
      Seq(
        {
          val squareInFront = BoardOps.findSquareInBoard(center.forward, board)
          if (squareInFront.isEmpty) Option apply Vector(center.forward)
          else None
        },  {
          val squareInFront = BoardOps.findSquareInBoard(center.forward, board)
          val squareInFrontFront = BoardOps.findSquareInBoard(center.forward.forward, board)
          val pawnInitialRow = t match {
            case Team.White => tableLowerBound + Team.perspective(t)
            case Team.Black => tableUpperBound + Team.perspective(t)
          }
          if (squareInFront.isEmpty && squareInFrontFront.isEmpty && center.y == pawnInitialRow)
            Option apply Vector(center.forward.forward)
          else None
        },{
          for {
            pieceToAttack <- BoardOps.findSquareInBoard(center.forward.right, board)
            ownPiece <- BoardOps.findSquareInBoard(center, board)
            attack <- if (pieceToAttack.team != ownPiece.team) Some(true)
            else None
          } yield Vector(center.forward.right)
        }, {
          for {
            pieceToAttack <- BoardOps.findSquareInBoard(center.forward.left, board)
            ownPiece <- BoardOps.findSquareInBoard(center, board)
            attack <- if (pieceToAttack.team != ownPiece.team) Some(true)
            else None
          } yield Vector(center.forward.left)
        }
      ).collect {
          case Some(v) => v
      }.map { vector =>
        vector.filter(BoardOps.existsInBoard)
      }
  }
}
