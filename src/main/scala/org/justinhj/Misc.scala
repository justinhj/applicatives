package org.justinhj

object Misc {
  import cats._
  import cats.implicits._

  def miffy[A, F[_]: Monad](mb: F[Boolean], fa: F[A], fb: F[A]): F[A] = {
    mb.flatMap{
      b =>
        if(b) fa
        else fb
    }
  }

  def iffy[A, F[_]: Applicative](mb: F[Boolean], fa: F[A], fb: F[A]): F[A] = {
    Applicative[F].map3(mb, fa, fb){
      case (cond, a, b) =>
        if(cond) a else b
    }
  }

  // miffy(Option(true), Option(1), None)
  // res: Option[Int] = Some(1)

  // iffy(Option(true), Option(1), None)
  // res: Option[Int] = None

  def main(args: Array[String]): Unit = {
  }
}