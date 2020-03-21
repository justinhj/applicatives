package org.justinhj

object ApplicativeEval {

  import cats.implicits._
  import cats._
  import cats.data.Reader
  import cats.syntax._

  // Evaluating expressions
  // Here's a simple expression evaluator

  // data Exp v = Var v
  // | Val Int
  // | Add (Exp v) (Exp v)
  // eval :: Exp v → Env v → Int
  // eval (Var x ) env = fetch x env
  // eval (Val i) env = i
  // eval (Add p q) env = eval p env + eval q env

  sealed trait Exp
  case class Val(value: Int) extends Exp
  case class Add(left: Exp, right: Exp) extends Exp
  case class Var(key: String) extends Exp

  case class Env[K](kv: Map[K,Int])

  def fetch(key: String)(env: Env[String]) : Int =
    env.kv.getOrElse(key, 0)

  def eval(exp: Exp, env: Env[String]) : Int = {
    exp match {
      case Val(value) => value
      case Var(key) => fetch(key)(env)
      case Add(left, right) =>
        eval(left, env) + eval(right, env)
    }
  }

  // We can eliminate the clutter of the explicitly threaded environment with a little
  // help from some very old friends (in the paper K and S, pure and ap respectively)

  // eval :: Exp v → Env v → Int
  // eval (Var x ) = fetch x
  // eval (Val i) = pure i
  // eval (Add p q) = ap(K (+), eval p).ap(eval q)

  def fetchR(key: String) = Reader[Map[String,Int], Int] {
   env =>
    env.getOrElse(key, 0)
  }
  def pureR(value: Int) = Reader[Map[String,Int], Int] {
   env =>
    value
  }
  def eval2(exp: Exp): Reader[Map[String,Int], Int] = {
    exp match {
      case Val(value) => pureR(value)
      case Var(key) => fetchR(key)
      case Add(left, right) =>
        val f = Reader((env:Map[String,Int]) =>
          (a:Int) => (b:Int) => a + b)

        val f1 = eval2(left).ap(f)

        val f2 = eval2(right).ap(f1)
            // .ap(eval2(left))
            // .ap(eval2(right))

        f2

    }
  }


  val f = Reader((env:Map[String,Int]) => (a:Int) => (b:Int) => a + b)
  //val f2 = Reader((env:Int) => (b:Int) => b)

  val fap = pureR(10).ap(f)

  def main(args: Array[String]): Unit = {

    val env1 = Env(Map("x" -> 3, "y" -> 10))
    val exp1 = Add(Val(10), Add(Var("x"), Var("y")))

    println(s"Eval : ${eval(exp1, env1)}")
  }

}