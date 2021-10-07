package domain

object Constants {
  val tableLowerBound = 1
  val tableUpperBound = 8
  val size: Seq[Int] = tableLowerBound to tableUpperBound
  val letters: Seq[String] = ('a' to 'h').toVector.map(_.toString)
  val filesExtension = "txt"

  def letterIndex(letter: String): Int = letters.indexOf(letter) + 1
  def letterAt(index: Int): String = letters(index - 1)

  val King = "K"
  val Queen = "Q"
  val Bishop = "B"
  val Knight = "N"
  val Rook = "R"
  val Pawn = "P"
}
