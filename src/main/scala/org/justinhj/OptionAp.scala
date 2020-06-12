package org.justinhj

import cats.implicits._
import cats.Applicative

object OptionAp extends App {

  // Increment an Option[Int]

  def inc(n : Int): Int = n + 1

  val incremented = Applicative[Option].pure(a => inc(a)).ap(10.some)

  println(s"incremented: $incremented")

  // incremented: Some(11)

  // Add two options
  def add(a : Int, b: Int): Int = a + b

  val added: Option[Int] = Applicative[Option].pure((a:Int) => b => add(a,b))
    .ap(10.some)
    .ap(20.some)

  println(s"added: $added")

  // added: Some(30)


}