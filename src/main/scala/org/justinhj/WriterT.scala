package org.justinhj

object WriterTOldSchool extends App  {

  // Implement WriterT using Cats implementation of Monad and Monoids

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

    // Simple default implementation of tailRecM
    override def tailRecM[A, B](a: A)(f: A => WriterT[F,W,Either[A,B]]): WriterT[F,W,B] =
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }

  }

  // Use an implicit class conversion to add flatMap and map as methods to any WriterT ...

  implicit final class WriterTOps[F[_]: Monad, W: Monoid, A](private val fa: WriterT[F,W,A]) {
    def flatMap[B](f: A => WriterT[F,W,B]): WriterT[F,W,B] =
      Monad[WriterT[F,W,?]].flatMap(fa)(a => f(a))

    def map[B](f: A => B): WriterT[F,W,B] =
      Monad[WriterT[F,W,?]].map(fa)(a => f(a))
  }

  def incrementEven(a: Int): WriterT[Either[String, ?],String,Int] = {
    if(a % 2 == 1) WriterT(Left[String, (String, Int)]("Odd number provided"))
    else WriterT(Right(("Inc even", a + 1)))
  }

  def doubleOdd(a: Int): WriterT[Either[String, ?], String, Int] = {
    if(a % 2 == 0) WriterT(Left[String, (String, Int)]("Even number provided"))
    else WriterT(Right(("Double odd", a + a)))
  }

  // Step 1 can we flatMap?

  val writerExample = incrementEven(8).flatMap(doubleOdd)

  println(writerExample)

  // Step 2 can we use pure

  val p8 = Monad[WriterT[Either[String, ?], String, ?]].pure(8)

  println(p8.flatMap(incrementEven).flatMap(doubleOdd))

  // Step 3 use in a for comprehension?

  val r : WriterT[Either[String, ?], String, Int] = for (
    a <- Monad[WriterT[Either[String, ?], String, ?]].pure(8);
    b <- incrementEven(a);
    c <- doubleOdd(b)
  ) yield c

  println(r)
}

