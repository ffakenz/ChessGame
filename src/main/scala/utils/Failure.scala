package utils

trait Failure

object Failure {
  def error[F <: Failure, A](f: F): Either[F, A] = Left(f)
  def success[F <: Failure, A](a: A): Either[F, A] = Right(a)
}
