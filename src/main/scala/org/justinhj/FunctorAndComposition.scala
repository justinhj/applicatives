package org.justinhj

import cats.Functor
import cats.data.Validated
import cats.implicits._
import cats.effect._
import cats.data.Validated
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.data.Nested


object FunctorAndComposition extends IOApp {

  type NelStringValidated[A] = Validated[NonEmptyList[String], A]

  val fio = Functor[NelStringValidated].compose[IO]

  def validIOTest(n: Int): NelStringValidated[IO[Int]] = {
    if(n % 2 == 0) {
      NonEmptyList.of("Even number provided").invalid
    } else {
      IO{
        println(s"Booba! $n")
        n
      }.valid
    }
  }

  def intToEnglish(a: Int) : String = {
    if(a == Int.MinValue) "very negative number"
    else if (a == 2) "two"
    else if (a == 3) "three"
    // and so on for every Int ...
    else // (a == Int.MaxValue)
      "very big number"
  }


  def run(args: List[String]): IO[ExitCode] = {

    val a1 = validIOTest(2)
    val a2 = validIOTest(1)

    val a1f = fio.map(a1) {
      n =>
        n + 1
    }

    val a2f = fio.map(a2) {
      n =>
        n + 1
    }

    val ios = List(a1f, a2f).mapFilter {
      case Valid(io) =>
        io.some
      case _ =>
        None
    }

    val what = ios.sequence

    what.map(_ => ExitCode.Success)

  }
}