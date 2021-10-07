package domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import Fixtures.dataset._
import Fixtures.scenarios
import utils.Failure._

class RulesSpec extends AnyWordSpec with Matchers {
  import DomainFailure._

  "same coordinate" should {
    import scenarios.single._
    "fail" in {
      Rules.completeMoveValidation(
        OnePawn,
        WhitePawn,
        WhitePawn.coordinate) must be (error(SameCoordinateFailure(WhitePawn)))
    }
  }

  "verify to is enemy or empty" should {
    "success" in {
      import scenarios.enemies._
      implicit val t: Team = Team.White
      Rules.completeMoveValidation(
        TwoEnemyRooks,
        WhiteLeftRook,
        BlackLeftRook.coordinate) must be(success())

      Rules.completeMoveValidation(
        TwoEnemyRooks,
        WhiteLeftRook,
        WhiteLeftRook.coordinate.right) must be(success())
    }
    "fail" in {
      import scenarios.friends._
      Rules.completeMoveValidation(
      TwoFriendRooks,
      WhiteLeftRook,
      WhiteRightRook.coordinate) must be (error(CannotAttackAlly(WhiteRightRook.coordinate)))
    }
  }

  "verify to is reachable from square" should {
    import scenarios.single._
    implicit val t: Team = Team.White
    "success" in {
      val expectedCoordinate = WhitePawn.coordinate.forward
      Rules.completeMoveValidation(
        OnePawn,
        WhitePawn, expectedCoordinate
      ) must be (success())
    }
    "fail" in {
      val expectedCoordinate = WhitePawn.coordinate.forward.forward.forward
      Rules.completeMoveValidation(
        OnePawn,
        WhitePawn, expectedCoordinate
        ) must be (error(SquareCannotReachDestination(WhitePawn, expectedCoordinate)))
    }
  }

  "verify trajectory is not blocked" should {
    import scenarios.trajectory._
    "success" in {
      val expectedCoordinate = WhiteLeftRook.coordinate.forward.forward
      Rules.completeMoveValidation(
        TwoFriendRooksNextToEachOther,
        WhiteLeftRook, expectedCoordinate
      ) must be (success())
    }
    "fail" in {
      val expectedCoordinate = leftRook.coordinate.right.right
      Rules.completeMoveValidation(
        TwoFriendRooksNextToEachOther,
        leftRook, expectedCoordinate
      ) must be (error(TrajectoryBlocked(leftRook, rightRook)))
    }
  }
}
