package org.justinhj

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.ExecutionContext

/**
 * ParallelEither
 * @return IOApp
 */
object ParallelEither extends IOApp {
  /**
   * The main application
   * @param args
   * @return
   */
  def run(args: List[String]): IO[ExitCode] = {

    implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

    // processing a list using parallel either

    val validate = List(1,1,2,3,4).parTraverse{
      case 3 =>
        Either.left(NonEmptyList.one("Don't like three"))
      case 1 =>
        Either.left(NonEmptyList.one("Don't like one"))
      case n =>
        Either.right(n)
    }

    IO(println(s"Validate is $validate")).map(_ => ExitCode.Success)
  }
}
