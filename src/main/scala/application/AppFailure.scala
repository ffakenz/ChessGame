package application

import utils.Failure

sealed trait AppFailure extends Failure
object AppFailure {
  case class UserInputParseError(input: String) extends AppFailure
  case class FileNotExists(fileName: String) extends AppFailure
}
