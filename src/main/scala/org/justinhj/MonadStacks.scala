package org.justinhj

import cats.data.WriterT
import cats.Monad
// import cats.data.EitherT
// import cats.Id
import cats.implicits._

object MonadStacks extends App {

  // This is an example of using the Cats WriterT and how much help the compiler needs...

  sealed trait EvalError
  case object InvalidSymboName extends EvalError
  case object SymbolNotFound extends EvalError
  case object DivisionByZero extends EvalError

  type EvalResult[A] = Either[EvalError, A]
  type EvalResultLogged[A] = WriterT[EvalResult[?], List[String], A]

  def incrementEven(a: Int): EvalResult[Int] = {
    if(a % 2 == 1) Left(SymbolNotFound)
    else Right(a + 1)
  }

  def doubleOdd(a: Int): EvalResult[Int] = {
    if(a % 2 == 0) Left(DivisionByZero)
    else Right(a + a)
  }

  val p8 = Monad[EvalResultLogged].pure(8).flatMap(f => WriterT.liftF(doubleOdd(f))).flatMap(f => WriterT.tell(List("Hello World")))
  println(s"p8 $p8")

  // Here we interleave operations on an Either with tells that have
  // access to intermediate computed values...
  val answer: EvalResultLogged[Int] = for (
    s1 <- Monad[EvalResultLogged].pure(8);
    _ <- WriterT.tell[EvalResult, List[String]](List(s"Imma do something with $s1"));
    s2 <- WriterT.liftF[EvalResult, List[String], Int](incrementEven(s1));
    _ <- WriterT.tell[EvalResult, List[String]](List(s"Now we have $s2"));
    s3 <- WriterT.liftF[EvalResult, List[String], Int](doubleOdd(s2))
  ) yield s3


  println(s"Log ${answer.written}")

}
