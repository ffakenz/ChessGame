package infrastructure

import application.Game
import domain.Constants._
import domain._

object Render {
  private type Matrix[A] = Vector[Vector[A]]

  private val pieceRender: Piece => String = {
    case Piece.King => King
    case Piece.Queen => Queen
    case Piece.Bishop => Bishop
    case Piece.Knight => Knight
    case Piece.Rook => Rook
    case Piece.Pawn => Pawn
  }

  def renderGame: Game => String = (game: Game) => {
    s"TURN: ${game.turn} \n" + (if(game.kingInCheck) "[YOUR KING IS IN CHECK]" else "") +
      (prettyPrintTable _ compose fromBoardToTable)(game.board)
  }

  private def pieceRenderByTeam(team: Team, p: Piece): String = {
    team match {
      case Team.White => pieceRender(p).toUpperCase()
      case Team.Black => pieceRender(p).toLowerCase()
    }
  }

  private def fromBoardToTable(board: Board): Matrix[String] = {
    size.reverse.toVector.map { y =>
      size.toVector.map { x =>
        BoardOps.findSquareInBoard(Coordinate(x, y), board) match {
          case Some(square) => pieceRenderByTeam(square.team, square.piece) +
            s"(${letterAt(square.coordinate.x)}${square.coordinate.y})"
          case None => "_" +
            s"(${letterAt(x)}$y)"
        }
      }
    }
  }

  private def prettyPrintTable(table: Matrix[String]): String = {
    table.map { row =>
      row.mkString("│_", "_│_", "_|")
    }.foldLeft("") { (render, rowString) =>
      render + "\n" + rowString
    }
  }
}