package org.justinhj

object WriterTOldSchool extends App  {

  import cats.{Monad, Monoid}

  case class WriterT[F[_]: Monad,W,A](val wrapped: F[(W,A)])

  implicit def writerTMonad[F[_]: Monad,W: Monoid] = new Monad[WriterT[F,W,?]] {

    override def pure[A](a: A): WriterT[F,W,A] = WriterT(Monad[F].pure((Monoid[W].empty,a)))

    override def flatMap[A, B](fa: WriterT[F,W,A])(f: A => WriterT[F,W,B]): WriterT[F,W,B] = {
      val ffa: F[(W,B)] = Monad[F].flatMap(fa.wrapped) {
        case (wa,a) => {
          val what = f(a).wrapped
          Monad[F].map(what){
            case (wb, b) =>
              (Monoid[W].combine(wa,wb), b)
          }
        }
      }
      WriterT(ffa)
    }

    override def tailRecM[A, B](a: A)(f: A => WriterT[F,W,Either[A,B]]): WriterT[F,W,B] = ???

  }

  implicit class WriterTOps[F[_]: Monad, W: Monoid, A](fa: WriterT[F,W,A]) {
    def flatMap[B](f: A => WriterT[F,W,B]): WriterT[F,W,B] =
      Monad[WriterT[F,W,?]].flatMap(fa)(a => f(a))
  }

  type StringEither[A] = Either[String, A]

  def incrementEven(a: Int): WriterT[StringEither,String,Int] = {
    if(a % 2 == 1) WriterT(Left[String, (String, Int)]("Odd number provided"))
    else WriterT(Right(("Inc even", a + 1)))
  }

  def doubleOdd(a: Int): WriterT[StringEither, String, Int] = {
    if(a % 2 == 0) WriterT(Left[String, (String, Int)]("Even number provided"))
    else WriterT(Right(("Double odd", a + a)))
  }

  val writerExample = incrementEven(8).flatMap(doubleOdd)

  println(writerExample)

}

