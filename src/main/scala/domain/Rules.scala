package domain

import domain.BoardOps._
import utils.Failure._

object Rules {
  import DomainFailure._

  def completeMoveValidation(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    for {
      _ <- validateMove(board, square, to)
      _ <- verifyCheck(board, square, to)
    } yield ()
  }

  def isKingCheck(board: Board, king: Square): Either[DomainFailure, Unit] = {
    val enemies = searchSquaresFromTeam(Team.enemy(king.team), board)
    val threateningEnemies = enemies.map { enemy =>
      isEnemyThreateningKing(board, enemy, king).map(_ => enemy)
    }
    threateningEnemies.foldLeft(success[DomainFailure, Unit]()) { case (result, validation) =>
      result.flatMap { _ =>
        validation match {
          case Left(_) => success()
          case Right(enemy) => error(EnemyThreatingKing(enemy, king))
        }
      }
    }
  }
  
  private def verifyCoordinateIsWithinBoard(to: Coordinate): Either[DomainFailure, Unit] = {
    if(BoardOps.existsInBoard(to)) success()
    else error(CoordinateOutOfBoard(to))
  }

  private def verifySquareExistsInBoard(board: Board, square: Square): Either[DomainFailure, Unit] = {
    findSquareInBoard(square.coordinate, board) match {
      case Some(_) => success()
      case None => error(SquareNotExists(square))
    }
  }

  private def verifyNotMoveToSameCoordinate(square: Square, to: Coordinate): Either[DomainFailure, Unit] =
    if (square.coordinate != to) success() else error(SameCoordinateFailure(square))

  private def verifyToIsEnemyOrEmpty(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    val maybeToSquare: Option[Square] = findSquareInBoard(to, board)
    maybeToSquare match {
      case Some(s) if s.team == square.team => error(CannotAttackAlly(to))
      case _ => success()
    }
  }

  private def verifyToIsReachableFromSquare(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    def findStamp(stamps: Vector[Vector[Coordinate]], coordinate: Coordinate): Option[Coordinate] = {
      stamps.find { row =>
        row.contains(coordinate)
      }.flatMap { row =>
        row.find(_ == coordinate)
      }
    }
    val candidates = searchPieceTrayectories(square, board)

    val maybeStamp = findStamp(candidates.toVector, to)
    maybeStamp match {
      case Some(_) => success()
      case None => error(SquareCannotReachDestination(square, to))
    }
  }

  private def verifyTrajectoryIsNotBlocked(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    val candidates = searchPieceTrayectories(square, board)
    val maybeCandidate: Option[Vector[Coordinate]] =
      candidates.find { candidate => candidate.contains(to) }.map(
        trajectoryCandidate =>
          trajectoryCandidate.takeWhile(_ != to) :+ to
      )

    maybeCandidate match {
      case Some(candidate) =>
        val failuresOrSuccess = candidate.map { candidateCoordinate =>
          val maybeSquare = findSquareInBoard(candidateCoordinate, board)
          maybeSquare match {
            case None => success()
            case Some(blocker) =>
              blocker match {
                case Square(coordinate, _, _) if coordinate == to => success()
                case _ => error(TrajectoryBlocked(square, blocker))
              }
          }
        }
        failuresOrSuccess.foldLeft(success[DomainFailure, Unit]()) { case (result, validation) =>
          for {
            _ <- result
            unit <- validation
          } yield unit
        }
      case None => error(SquareCannotReachDestination(square, to)) // todo check this logic might be redundant
    }
  }

  private def isEnemyThreateningKing(board: Board, enemy: Square, king: Square): Either[DomainFailure, Unit] = {
    validateMoveForCheck(board, enemy, king.coordinate)
  }

  private def verifyCheck(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    if (Piece.isKing(square.piece)) {
      val tmpBoard = Board(board.pieces - square.coordinate)
      val kingMoved = square.copy(coordinate = to)
      val boardWithKingMoved = Board(tmpBoard.pieces + (to -> kingMoved))
      isKingCheck(boardWithKingMoved, kingMoved)
    } else {
      val yourKing = board.pieces.values.find(s => Piece.isKing(s.piece) && s.team == square.team)
      yourKing match {
        case Some(king) =>
          for {
            _ <- isKingCheck(board, king)
            tmpBoard = Board(board.pieces - square.coordinate)
            pieceMoved = square.copy(coordinate = to)
            boardWithPieceMoved = Board(tmpBoard.pieces + (to -> pieceMoved))
            _ <- isKingCheck(boardWithPieceMoved, king)
          } yield ()
        case None => error(KingDoesNotExist)
      }
    }
  }

  private def validateMove(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    for {
      _ <- verifyNotMoveToSameCoordinate(square, to)
      _ <- verifyCoordinateIsWithinBoard(to)
      _ <- verifySquareExistsInBoard(board, square)
      _ <- verifyToIsEnemyOrEmpty(board, square, to)
      _ <- verifyToIsReachableFromSquare( board, square, to)
      _ <- verifyTrajectoryIsNotBlocked(board, square, to)
    } yield ()
  }

  private def validateMoveForCheck(board: Board, square: Square, to: Coordinate): Either[DomainFailure, Unit] = {
    for {
      _ <- verifyToIsReachableFromSquare( board, square, to)
      _ <- verifyTrajectoryIsNotBlocked(board, square, to)
    } yield ()
  }
}
