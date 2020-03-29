package org.justinhj

import zio._
import zio.console._
import zio.clock._
import zio.duration._
import cats.Applicative
import cats.implicits._

object MyApp extends App {

  def run(args: List[String]) = {
    (for (
      _ <- putStrLn("Applicative version\n");
      _ <- myAppLogicParallel;
      _ <- putStrLn("Monadic version\n");
      _ <- myAppLogicSequence
    ) yield ()).fold(_ => 1, _ => 0)
  }

  // Here's a ZIO effect that will wait the specified number of seconds then
  // say the number of seconds
  def delayedPrintNumber(s: Int): ZIO[Console with Clock,String,Int] = {
    putStrLn(s"Preparing to say number in $s seconds") *>
    putStrLn(s"$s").delay(s.seconds) *>
    ZIO.succeed(s)
  }

  implicit def zioApplicative[Z,E] = new Applicative[ZIO[Z,E,?]] {
    def pure[A](x: A) = ZIO.succeed(x)
    def ap[A, B](ff: ZIO[Z,E,A => B])(fa: ZIO[Z,E,A]) = {
      map2(ff, fa){
        (f,a) =>
          f(a)
      }
    }
    override def map2[A, B, C](fa: zio.ZIO[Z,E,A], fb: zio.ZIO[Z,E,B])(f: (A, B) => C):
      zio.ZIO[Z,E,C] = {
        fa.zipPar(fb).map{case (a,b) => f(a,b)}
    }
  }

  def monadicSequence[Z,E,A](ios: List[ZIO[Z, E, A]]): ZIO[Z, E, List[A]] = {
    ios match {
      case Nil =>
        zioApplicative.pure(List.empty[A])
      case c :: cs =>
        for (
          x <- c;
          xs <- monadicSequence(cs)
        ) yield (x +: xs)
    }
  }

  def applicativeSequence[Z,E,A](ios: List[ZIO[Z, E, A]]): ZIO[Z, E, List[A]] = {
    ios match {
      case Nil =>
        ZIO.succeed(List.empty[A])
      case c :: cs =>
        val ff: ZIO[Z,E, A => (List[A] => List[A])] =
          zioApplicative.pure(((a: A) => (listA: List[A]) => a +: listA))
        val p1 = ff.ap(c)
        p1.ap(applicativeSequence(cs))
    }
  }

  val ios1 = List(6,5,2,1,3,8,4,7).map(delayedPrintNumber)

  val myAppLogicParallel = {
    applicativeSequence(ios1)
  }

  val myAppLogicSequence = {
    monadicSequence(ios1)
  }
}