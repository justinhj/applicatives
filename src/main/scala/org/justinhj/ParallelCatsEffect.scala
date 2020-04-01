package org.justinhj

object ParallelCatsEffect {
  import cats.implicits._
  import cats.data._
  import cats.effect.{ContextShift, Timer, IO}
  import scala.concurrent.ExecutionContext

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  // Here we explicitly use the IO's applicative
  val sampleIO = IO((a: Int) => a + 1)
                    .ap(IO(1))
                    .flatMap(n => IO(println(s"hello $n")))

  val aLotOfIOs =
    NonEmptyList.of(sampleIO, sampleIO)


  val ioOfList = aLotOfIOs.parSequence

  def main(args: Array[String]): Unit = {
    ioOfList.unsafeRunSync()
  }

}
