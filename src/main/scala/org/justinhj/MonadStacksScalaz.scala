package org.justinhj

import scalaz._
import std.AllInstances._
import syntax.all._

object MonadStacksScalaz extends App {

  sealed trait EvalError
  case object InvalidSymboName extends EvalError
  case object SymbolNotFound extends EvalError
  case object DivisionByZero extends EvalError

  type EvalResult[A] = Either[EvalError, A]
  type EvalResultLogged[A] = WriterT[List[String], EvalResult[?], A]

  def incrementEven(a: Int): EvalResult[Int] = {
    if(a % 2 == 1) Left(SymbolNotFound)
    else Right(a + 1)
  }

  def doubleOdd(a: Int): EvalResult[Int] = {
    if(a % 2 == 0) Left(DivisionByZero)
    else Right(a + a)
  }

  // Pure works
  val p8 = Monad[EvalResultLogged].pure(8)
  println(s"p8 ${p8}")
  
  // WriterT with two arguments not one 
  type StringsWriter[F[_], A] = WriterT[List[String],F,A]

  // Flatmaps in sequence
  println(
    p8.flatMap(a => incrementEven(a).liftM[StringsWriter])
      .flatMap(a => doubleOdd(a).liftM[StringsWriter]))

  // For comprehension
  def program1(n: Int): EvalResultLogged[Int] = 
    for (
      a <- incrementEven(n).liftM[StringsWriter];
      c <- WriterT.putWith(Monad[EvalResult].pure(a))(a1 => List(s"Incremented to $a1"));
      b <- doubleOdd(a).liftM[StringsWriter]
    ) yield a

  println(program1(10))
}
