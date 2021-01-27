package org.justinhj

import zio._
import zio.console._
import zio.clock._
import zio.duration._

object ZioScheduleExample extends App {

  val everySecond = Schedule.spaced(1.second)
  val everyThreeSeconds = Schedule.spaced(3.seconds)
  val everyFiveSeconds = Schedule.spaced(5.seconds)

  def run(args: List[String]) = {
    val fizz = putStrLn("Fizz")
    val buzz = putStrLn("Buzz")
    val timeInSeconds = clock.nanoTime.map(_ / 1000000000L)

    for (
      start <- timeInSeconds;
      _ <- (ZIO.sleep(3.seconds).flatMap(_ => fizz.repeat(everyThreeSeconds))).fork;
      _ <- (ZIO.sleep(5.seconds) *> buzz.repeat(everyFiveSeconds)).fork;
      _ <- timeInSeconds.flatMap(time => putStrLn(s"time ${time - start}")).repeat(everySecond)
    ) yield ExitCode.success
  }
}
