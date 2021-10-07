package domain

import utils.Failure

sealed trait DomainFailure extends Failure
object DomainFailure {
  case class SameCoordinateFailure(s: Square) extends DomainFailure
  case class SquareNotExists(s: Square) extends DomainFailure
  case class CannotAttackAlly(c: Coordinate) extends DomainFailure
  case class SquareCannotReachDestination(s: Square, c: Coordinate) extends DomainFailure
  case class CoordinateOutOfBoard(c: Coordinate) extends DomainFailure
  case class TrajectoryBlocked(blocked: Square, blocker: Square) extends DomainFailure
  case class EnemyThreatingKing(enemy: Square, king: Square) extends DomainFailure
  case object KingDoesNotExist extends DomainFailure
}
