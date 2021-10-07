package domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import Fixtures.dataset._
import Fixtures.scenarios

class BoardSpec extends AnyWordSpec with Matchers {

  import scenarios.empty._
  import scenarios.single._

  "BoardOps" should {
    "the coordinate you are looking for is missing from board" in {
      BoardOps.findSquareInBoard(Coordinate(99, 99), EmptyBoard) must be(None)
      BoardOps.findSquareInBoard(Coordinate(1,1), OnePawn) must be(None)
    }
    "the coordinate you are looking for is found in board" in {
      BoardOps.findSquareInBoard(WhitePawn.coordinate, OnePawn) must be(Some(WhitePawn))
    }
    "search squares from team" in {
      BoardOps.searchSquaresFromTeam(Team.White, OnePawn) must be(Vector(WhiteKing, WhitePawn))
      BoardOps.searchSquaresFromTeam(Team.Black, OnePawn) must be(Vector(BlackKing))
    }
    "search pieces trajectories" in {
      val expectedTrajectories = Moves.PawnMoves.getMoves(WhitePawn.coordinate, OnePawn)(WhitePawn.team)
      BoardOps.searchPieceTrayectories(WhitePawn, OnePawn) must be(
        expectedTrajectories
      )
    }
  }
}
