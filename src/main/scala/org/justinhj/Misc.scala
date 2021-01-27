package org.justinhj

object Misc {
  import cats._
  import cats.implicits._

  //val i1 = new IntOrString.IsInt(10)

  // i1.IsInt()

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

  def map5[A,B,C,D,E,Z, F[_]: Applicative](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])
    (f: (A,B,C,D,E) => Z): F[Z] = {
      Applicative[F].pure((a: A) => (b: B) => (c: C) => (d: D) => (e: E) => f(a,b,c,d,e)).
        ap(fa).
        ap(fb).
        ap(fc).
        ap(fd).
        ap(fe)
  }

  def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
    as match {
      case a :: rest => G.map2(f(a), traverse(rest)(f))(_ +: _)
      case Nil => G.pure(List.empty[B])
    }
  }

  // miffy(Option(true), Option(1), None)
  // res: Option[Int] = Some(1)

  // iffy(Option(true), Option(1), None)
  // res: Option[Int] = None

  def main(args: Array[String]): Unit = {
  }
}