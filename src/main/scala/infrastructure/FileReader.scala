package infrastructure

import java.io.File

import application.AppFailure
import domain.Constants._
import com.whitehatgaming.UserInputFile
import domain.{Coordinate, Move}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import AppFailure._
import utils.Failure._

object FileReader {
  def getListOfFiles(dir: String): Either[AppFailure, List[File]] = {
    Try(new File(dir).listFiles) match {
      case Failure(_) => error(FileNotExists(dir))
      case Success(files) =>
        success(files.filter(_.isFile).toList.filter { file =>
          file.getName.endsWith(filesExtension)
        })
    }
  }

  def readMoves(): List[Move] = {
    @tailrec
    def readAll(userInputFile: UserInputFile, moves: List[Move] = List.empty): List[Move] = {
      readMove(userInputFile) match {
        case Left(_) => moves // case end of file
        case Right(Some(move)) => readAll(userInputFile, moves :+ move)
        case Right(None) => readAll(userInputFile, moves) // case skip wrong move
      }
    }
    val fileName = io.StdIn.readLine()
    val userInputFile: UserInputFile = new UserInputFile(fileName)
    readAll(userInputFile)
  }

  private def readMove(userInputFile: UserInputFile): Either[Unit, Option[Move]] = {
    val maybeMoves: Option[Array[Int]] = Option(userInputFile.nextMove())
    maybeMoves match {
      case None => Left(())
      case Some(move) =>
        val fromTo = for {
          fromX <- move.headOption
          fromY <- move.tail.headOption
          toX <- move.tail.tail.headOption
          toY <- move.lastOption
        } yield Seq(fromX + 97, 56 - fromY, toX + 97, 56 - toY)

        val maybeMove = fromTo.flatMap { elements =>
          val chars = elements.map(_.toChar).map(_.toString)
          for {
            fromX <- chars.headOption
            fromY <- chars.tail.headOption
            toX <- chars.tail.tail.headOption
            toY <- chars.lastOption
          } yield Move(Coordinate(letterIndex(fromX), fromY.toInt), Coordinate(letterIndex(toX), toY.toInt))
        }
        Right(maybeMove)
    }
  }
}
