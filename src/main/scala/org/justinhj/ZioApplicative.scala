package org.justinhj

import zio._
import zio.console._
import zio.clock._
import zio.duration._
import cats.Applicative
import cats.implicits._

object MyApp extends App {

  def run(args: List[String]) =
    myAppLogicParallel.fold(_ => 1, _ => 0)

  // Here's a ZIO effect that will wait the specified number of seconds then
  // say hello, but if you give it an odd number it will throw an error. This
  // will let us demonstrate the applicative sequence
  def sayHelloMaybe(s: Int): ZIO[Console with Clock,String,Unit] = {
    if(s % 2 == 1)
      ZIO.fail(s"Got an odd number! ($s)")
    else
      putStrLn(s"Preparing to say hello in $s seconds").flatMap {
        _ =>
          putStrLn(s"Hello after ${s}").delay(s.seconds)
        }
  }

  implicit def zioApplicative[Z,E] = new Applicative[ZIO[Z,E,?]] {
    def pure[A](x: A) = ZIO.succeed(x)
    def ap[A, B](ff: ZIO[Z,E,A => B])(fa: ZIO[Z,E,A]) = {
      map2(ff, fa){
        (f,a) =>
          f(a)
      }
    }
    override def map2[A, B, C](fa: zio.ZIO[Z,E,A], fb: zio.ZIO[Z,E,B])(f: (A, B) => C): zio.ZIO[Z,E,C] = {
      fa.zipPar(fb).map{case (a,b) => f(a,b)}
    }
  }

  def applicativeSequence[Z,E,A](ios: List[ZIO[Z, E, A]]): ZIO[Z, E, List[A]] = {
    ios match {
      case Nil =>
        ZIO.succeed(List.empty[A])
      case c :: cs =>
        val w1: ZIO[Z,E, A => (List[A] => List[A])] =
          zioApplicative.pure(((a: A) => (listA: List[A]) => a +: listA))
        val w2 = w1.ap(c)
        val w3 = w2.ap(applicativeSequence(cs))
        w3
    }
  }

  val myAppLogicParallel: ZIO[Console with Clock,String, List[Unit]] = {
    val ios = List(
      sayHelloMaybe(10),
      sayHelloMaybe(4))

    applicativeSequence(ios)
  }

  val myAppLogicSequence =
    for {
      _ <- sayHelloMaybe(10);
      _ <- sayHelloMaybe(4)
    } yield ()
}