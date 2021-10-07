package domain

import Constants._

object BoardOps {
  def searchSquaresFromTeam(team: Team, b: Board): Vector[Square] = b.pieces.values.collect {
    case square if team == square.team => square
  }.toVector

  def searchPieceTrayectories(square: Square, board: Board): Seq[Vector[Coordinate]] = {
    Moves.getMoves(square.piece).getMoves(square.coordinate, board)(square.team)
  }

  def findSquareInBoard(coordinate: Coordinate, board: Board): Option[Square] =
    board.pieces.get(coordinate)

  def existsInBoard(coordinate: Coordinate): Boolean =
    coordinate.x <= tableUpperBound && coordinate.y <= tableUpperBound &&
      coordinate.x >= tableLowerBound && coordinate.y >= tableLowerBound
}
