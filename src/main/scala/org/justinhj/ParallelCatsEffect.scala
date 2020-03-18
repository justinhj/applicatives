package org.justinhj

object ParallelCatsEffect {
  import cats.implicits._
  import cats.data._
  import cats.effect.{ContextShift, Timer, IO}
  import scala.concurrent.ExecutionContext

  // Needed for IO.start to do a logical thread fork
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  val anIO = IO(1)

  val hello = IO.pure((a: Int) => a + 1).ap(anIO)

  val aLotOfIOs =
    NonEmptyList.of(anIO, anIO)

  val ioOfList = aLotOfIOs.parSequence_
}
