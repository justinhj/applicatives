package org.justinhj

object WriterTOldSchool extends App  {

  import cats.{Monad, Monoid}

  case class WriterT[F[_]: Monad,W,A](val wrapped: F[(W,A)])

  implicit def writerTMonad[F[_]: Monad,W: Monoid] = new Monad[WriterT[F,W,?]] = {
    def flatMap[A, B](fa: Monad[WriterT[F,W,A]])(f: A => Monad[WriterT[F,W,B]]): Monad[WriterT[F,W,B]] = ???
    def pure[A](x: A): F[A] = ???

  }


  // given writerTMonad[F[_]: Monad,W: Monoid]: Monad[[X] =>> WriterT[F,W,X]] with {

  //    def pure[A](a: A): WriterT[F,W,A] = WriterT(summon[Monad[F]].pure((Monoid[W].zero,a)))

  //    extension [A,B](fa: WriterT[F,W,A]) def fflatMap(f: A => WriterT[F,W,B]) = {
  //      val ffa: F[(W,B)] = Monad[F].fflatMap(fa.wrapped) {
  //        case (wa,a) => {
  //          f(a).wrapped.fmap {
  //            case (wb, b) =>
  //              (Monoid[W].combine(wa,wb), b)
  //          }
  //        }
  //      }
  //      WriterT(ffa)
  //    }
  // }

  println("Hello")


}

