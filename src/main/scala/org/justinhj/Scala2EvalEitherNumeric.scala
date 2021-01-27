package org.justinhj

import cats.implicits._

object Scala2Numeric {

  object Numeric {
    // This can be used to summon a numeric (same as implicitly)
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

    object ops {

      implicit class NumericOps[T](a: T)(implicit n: Numeric[T]) {
        def add(b: T): T = n.add(a, b)

        def +(b: T): T = n.add(a, b)

        def mul(b: T): T = n.mul(a, b)

        def *(b: T): T = n.mul(a, b)
      }

    }

  }

  trait Numeric[T] {
    def add(a: T, b: T): T

    def mul(a: T, b: T): T

    def square(a: T): T = mul(a, a)
  }

  implicit val numericInt: Numeric[Int] = new Numeric[Int] {

    def add(a: Int, b: Int): Int = a + b

    def mul(a: Int, b: Int): Int = a * b
  }

  implicit val numericLong: Numeric[Long] = new Numeric[Long] {

    def add(a: Long, b: Long): Long = a + b

    def mul(a: Long, b: Long): Long = a * b

  }

  implicit val numericString: Numeric[String] = new Numeric[String] {

    def add(a: String, b: String): String = a ++ b

    def mul(a: String, b: String): String = {
      for (as <- a;
           bs <- b;
           s <- as.toString ++ bs.toString) yield s
    }

  }
}

object Scala2EvalEitherNumeric extends App {

    // An expression evaluator using Scala 2

    import Scala2Numeric._
    import Scala2Numeric.Numeric.ops._

    sealed trait Exp[T]
    case class Val[T](value: T) extends Exp[T]
    case class Add[T](left: Exp[T], right: Exp[T]) extends Exp[T]
    case class Mul[T](left: Exp[T], right: Exp[T]) extends Exp[T]
    case class Var[T](identifier: String) extends Exp[T]

    type Env[T] = Map[String, T]

    sealed trait EvalError
    case object DivisionByZero extends EvalError
    case class UsedUndefinedSymbol(symbolName : String) extends EvalError

    type EvalResult[T] = Either[EvalError, T]

    implicit def numericEvalResult[T : Numeric] = new Numeric[EvalResult[T]] {

      def add(a: EvalResult[T], b: EvalResult[T]): EvalResult[T] = {
        val n = implicitly[Numeric[T]]
        a.map2(b)(n.add(_, _))
      }

      def mul(a: EvalResult[T], b: EvalResult[T]): EvalResult[T] = {
        val n = implicitly[Numeric[T]]
        a.map2(b)(n.mul(_, _))
      }
    }

    def eval[T : Numeric](exp: Exp[T])(implicit env : Env[T]): EvalResult[T] = {
      exp match {
        case Var(id) => handleVar(id)
        case Val(value) => Right(value)
        case Add(l,r) => handleAdd(l,r)
        case Mul(l,r) => handleMul(l,r)
      }
    }

    def handleAdd[T](l: Exp[T], r: Exp[T])(implicit env : Env[T], numeric: Numeric[T]): EvalResult[T] = eval(l) + eval(r)

    def handleMul[T](l: Exp[T], r: Exp[T])(implicit env : Env[T], numeric: Numeric[T]): EvalResult[T] = eval(l) * eval(r)
    def handleVar[T](s: String)(implicit env: Env[T]): EvalResult[T] = {
      env.get(s) match {
        case Some(t) => Right(t)
        case _ => Left(UsedUndefinedSymbol(s))
      }

    }

    val exp1 : Exp[Int] = Mul(Var("z"), Add(Val(30), Mul(Var("x"), Var("y"))))

    implicit val intEnv : Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)
    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")

    implicit val longEnv : Env[Long] = Map("x" -> 17, "y" -> 10, "z" -> 2)
    val eval2 = eval(Mul(Val(3L), Val(10L)))

    println(s"Eval2 exp gives $eval2")

    implicit val stringEnv : Env[String] = Map("x" -> "a", "y" -> "b")
    val eval3 = eval(Mul(Val("ab"), Val("cd")))

    println(s"Eval3 exp gives $eval3")

    // demonstrate a missing symbol
    {
      val exp1 : Exp[Int] = Mul(Var("z"), Add(Val(30), Mul(Var("x"), Var("y"))))

      implicit val intEnv : Env[Int] = Map("x" -> 17, "z" -> 2)
      val eval1 = eval(exp1)

      println(s"Eval exp gives $eval1")
    }

}
