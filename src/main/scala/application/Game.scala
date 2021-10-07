package application

import domain.{Board, BoardOps, Coordinate, Move, Piece, Rules, Team}
import infrastructure.{FileReader, Render}
import domain.Constants._
import AppFailure._
import utils.Failure._

import scala.annotation.tailrec

case class Game(board: Board, turn: Team, kingInCheck: Boolean, checkMate: Boolean)

object Game {
  val initialBoard: Board = Board.startNew()
  def start(): Unit = {
    val newGame = init()
    val gameAfterFileInput: Game = processMovesFromFile(newGame)

    @tailrec
    def processUserInput(g: Game): Unit = {
      val newGame = processMovesFromUser(g)
      if(newGame.checkMate) println("CHECK MATE!!")
      else processUserInput(newGame)
    }

    processUserInput(gameAfterFileInput)
  }

  private def init(): Game = {
    println(s"New Game Started")
    val initTeam = Team.White
    val boardGame = initialBoard
    val game = Game(boardGame, initTeam, kingInCheck = false, checkMate = false)
    println(Render.renderGame(game))
    println()
    game
  }

  private def processMove(game: Game, move: Move): Game = {
    BoardOps.findSquareInBoard(move.from, game.board) match {
      case Some(square) =>
        Rules.completeMoveValidation(game.board, square, move.to) match {
          case Left(failure) =>
            println(s"move $move failed due to $failure")
            println(Render.renderGame(game))
            game
          case Right(_) =>
            println(s"move $move succeed")
            val newBoard = game.board.copy(pieces = (game.board.pieces - square.coordinate) + (move.to -> square.copy(coordinate = move.to)))
            val newGame = game.copy(board = newBoard, Team.enemy(game.turn))
            val enemyKing = BoardOps.searchSquaresFromTeam(Team.enemy(game.turn), newBoard).find(s => Piece.isKing(s.piece)).get
            println(Render.renderGame(newGame))
            Rules.isKingCheck(newBoard, enemyKing) match {
              case Left(_) =>
                println(s"King ${enemyKing.team} is on CHECK !!")
                newGame.copy(kingInCheck = true)
              case Right(_) =>
                newGame
            }
        }
      case None =>
        println(s"move $move failed due to square not found in move from")
        println(Render.renderGame(game))
        game
    }
  }

  private def processMovesFromFile(game: Game): Game = {
    println(s"Please select moves file to process")
    println()
    val listedFiles = FileReader.getListOfFiles("./ProgTest/data/")
    listedFiles match {
      case Left(failure) =>
        println(s"Failed to process moves from file due to $failure")
        game
      case Right(files) =>
        println(s"${files.mkString("\n")}")
        val moves = FileReader.readMoves()
        moves.foldLeft(game) { case (game, move) =>
          if(game.checkMate) game
          else processMove(game, move)
        }
    }
  }

  private def parseUserInput(rawUserInput: String): Either[AppFailure, Move] = {
    rawUserInput.toCharArray.map(_.toString).mkString(",") match {
      case s"$fromLetter,$fromNumber,$toLetter,$toNumber" =>
        success(
          Move(
            Coordinate(letterIndex(fromLetter), fromNumber.toInt),
            Coordinate(letterIndex(toLetter), toNumber.toInt)
          )
        )
      case _ => error(UserInputParseError(rawUserInput))
    }
  }

  private def processMovesFromUser(game: Game): Game = {
    val rawUserInput = io.StdIn.readLine()
    val userInput = parseUserInput(rawUserInput)
    userInput match {
      case Right(move) => processMove(game, move)
      case Left(appFailure) =>
        println(s"move $rawUserInput failed due to $appFailure")
        println(Render.renderGame(game))
        game
    }
  }
}