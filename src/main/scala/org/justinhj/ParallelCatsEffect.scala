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

  val hello = IO((a: Int) => a + 1).ap(anIO).flatMap(n => IO(println(s"hello $n")))

  val aLotOfIOs =
    NonEmptyList.of(hello, hello)

  val ioOfList = aLotOfIOs.parSequence

  def main(args: Array[String]): Unit = {
    ioOfList.unsafeRunSync()
  }

}
