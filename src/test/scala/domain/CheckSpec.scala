package domain

import domain.Fixtures.dataset._
import domain.Fixtures.scenarios
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import utils.Failure._

class CheckSpec extends AnyWordSpec with Matchers {
  import DomainFailure._

  "check if moving king" should {
    import scenarios.base._
    implicit val team: Team = Team.White
    "success" in {
      Rules.completeMoveValidation(
        BoardWithKings,
        WhiteKing,
        WhiteKing.coordinate.forward) must be (success())
    }
    "fail" in {
      val enemy = BlackPawn.copy(coordinate = WhiteKing.coordinate.forward)
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy)),
        WhiteKing,
        WhiteKing.coordinate.right) must be (error(EnemyThreatingKing(enemy, WhiteKing.copy(coordinate = WhiteKing.coordinate.right))))
    }
  }

  "check if moving other piece than king" should {
    import scenarios.base._
    implicit val team: Team = Team.White
    "success" in {
      val friend = WhitePawn.copy(coordinate = WhiteKing.coordinate.left)
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(friend)),
        friend,
        friend.coordinate.forward) must be (success())
    }
    "fail because currently the king is in check" in {
      val enemy = BlackPawn.copy(coordinate = WhiteKing.coordinate.forward.right)
      val friend = WhitePawn.copy(coordinate = WhiteKing.coordinate.left)
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy, friend)),
        friend,
        friend.coordinate.forward) must be (error(EnemyThreatingKing(enemy, WhiteKing)))
    }
    "fail because currently the king is going to be in check" in {
      val enemy = BlackLeftRook.copy(coordinate = WhiteKing.coordinate.right.right)
      val friend = WhitePawn.copy(coordinate = WhiteKing.coordinate.right)
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy, friend)),
        friend,
        friend.coordinate.forward) must be (error(EnemyThreatingKing(enemy, WhiteKing)))
    }
  }

  "king attacks piece that is performing the check" should {
    import scenarios.base._
    implicit val team: Team = Team.White
    "success" in {
      val enemy = BlackLeftRook.copy(coordinate = WhiteKing.coordinate.right)
      val king = WhiteKing
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy)),
        king,
        enemy.coordinate) must be (success())
    }
    "fail because piece that is performing the check is defended by other piece" in {
      val enemy = BlackLeftRook.copy(coordinate = WhiteKing.coordinate.right)
      val secondEnemy = BlackLeftRook.copy(coordinate = enemy.coordinate.right)
      val king = WhiteKing
      val movedKing = king.copy(coordinate = enemy.coordinate)
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy, secondEnemy)),
        king,
        movedKing.coordinate) must be (error(EnemyThreatingKing(secondEnemy, movedKing)))
    }
  }

  "king escapes from piece that is performing the check" should {
    import scenarios.base._
    implicit val team: Team = Team.White
    "success" in {
      val enemy = BlackLeftRook.copy(coordinate = WhiteKing.coordinate.right)
      val king = WhiteKing
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy)),
        king,
        king.coordinate.forward) must be (success())
    }
    "fail because there is another enemy waiting where the king escapes" in {
      val king = WhiteKing
      val enemy = BlackLeftRook.copy(coordinate = king.coordinate.right)
      val secondEnemy = BlackRightRook.copy(coordinate = king.coordinate.right.forward)
      val movedKing = king.copy(coordinate = king.coordinate.forward)
      Rules.completeMoveValidation(
        BoardWithKings.copy(pieces = BoardWithKings.pieces ++ map(enemy, secondEnemy)),
        king,
        movedKing.coordinate) must be (error(EnemyThreatingKing(secondEnemy, movedKing)))
    }
  }
}
