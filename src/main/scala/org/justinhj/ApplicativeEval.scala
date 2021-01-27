package org.justinhj

object ApplicativeEval {

  import cats._
  //import cats.instances.AllInstances
  import cats.implicits._
  import cats.data.Reader
  import cats.Traverse

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

  def fetch(key: String)(env: Env[String]) = env.kv.getOrElse(key, 0)

  def eval(exp: Exp, env: Env[String]) : Int = {
    exp match {
      case Val(value) => value
      case Var(key) => fetch(key)(env)
      case Add(left, right) => eval(left, env) + eval(right, env)
    }
  }

  // "We can eliminate the clutter of the explicitly threaded environment with a little
  // help from some very old friends" (in the paper K and S are pure and ap respectively)

  // eval :: Exp v → Env v → Int
  // eval (Var x ) = fetch x
  // eval (Val i) = pure i
  // eval (Add p q) = ap(K (+), eval p).ap(eval q)

  type EnvReader[A] = Reader[Map[String,Int], A]
  def fetchR(key: String) = Reader[Map[String,Int], Int](env => env.getOrElse(key, 0))

  def evalR(exp: Exp): Reader[Map[String,Int], Int] = {
    exp match {
      case Val(value) => Applicative[EnvReader].pure(value)
      case Var(key) => fetchR(key)
      case Add(left, right) =>
        val f = Applicative[EnvReader].pure((a:Int) => (b:Int) => a + b)
        val l = evalR(left)
        val r = evalR(right)
        f.ap(l).ap(r)
    }
  }

  // Since exp is not a list we need a Traverse for Env

  sealed trait Exp2[+A]
  case class Val2[A](value: Int) extends Exp2[A]
  case class Add2[A](left: Exp2[A], right: Exp2[A]) extends Exp2[A]
  case class Var2[A](key: A) extends Exp2[A]

  // eval :: Exp v → Env v → Int
  // eval (Var x ) = fetch x
  // eval (Val i) = pure i
  // eval (Add p q) = pure + (eval p) (eval q)

  // In sequence the input is a list of F[A]
  // the output is a F[List[A]]
  // the applicative instance is F (the IO)

  // In eval the input is a list of Reader => Int

  type Env2[K] = Map[K,Int]

  def fetchR2[K](name: K) = Reader[Env2[K], Int] {
    env =>
      env(name)
  }

  type KEnvReader[K] = Reader[Env2[K], Int]

  def evalR2[K](exp: Exp2[K]) : Reader[Env2[K], Int] = {
    exp match {
      case Var2(x) => fetchR2(x)
      case Val2(value) => Applicative[Reader[Env2[K],?]].pure(value)
      case Add2(left, right) =>
        val f = Applicative[Reader[Env2[K],?]].pure((a:Int) => (b:Int) => a + b)
        val l = evalR2(left)
        val r = evalR2(right)
        f.ap(l).ap(r)
    }
  }

  // Can we do the above with sequence or traverse?

  // No, because with sequence and traverse we need to go from F[G[_]] to G[F[_]]
  // and the eval expression function does not have that signature. It is also
  // tied both to integer addition and output, as well as needing to be specialised
  // for the Reader data type

  // Question: Not sure why ap is reversed for Reader but this has to be written backwards
  // Update: It's a bug in Cats, I fixed it June 10

  // https://github.com/typelevel/cats/commit/d4f81766b8386cd4742df753054d19984cf2bd8d
  // changed order of parameter lists for Apply.ap
  // Clearer if you use map2
  // Applicative[EnvReader].map2(l,r){(a,b) => a + b}

  // Aside: You can see that the arity of the function in the List here determines
  // how many lists you can apply the function to. The first ap curries the application
  // of the function and the second completes it. A third would be an error.

  // @ List((a:Int) => (b:Int) => a + b, (a:Int) => (b:Int) => a - b).ap(List(1,2,3)).ap(List(4,5,6))
  // res15: List[Int] = List(5, 6, 7, 6, 7, 8, 7, 8, 9, -3, -4, -5, -2, -3, -4, -1, -2, -3)

  // @ Option((a:Int) => (b:Int) => a + b).ap(Some(1)).ap(Some(10))
  // res18: Option[Int] = Some(11)

  // if you have ap and map you have zip

  def zip[F[_] : Applicative, A, B](fa: F[A], fb: F[B]): F[(A,B)] = {
    fa.map(a => (b: B) => (a,b)).ap(fb)
  }

  def main(args: Array[String]): Unit = {

    val envMap = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val exp = Add(Val(10), Add(Var("x"), Var("y")))

    println(s"Eval : ${eval(exp, Env(envMap))}")
    // Eval : 23

    println(s"EvalR : ${evalR(exp).run(envMap)}")
    // EvalR : 23

    val exp2 : Exp2[String] =
      Add2(
        Var2("z"),
        Add2(
          Val2(10),
          Add2(
            Var2("x"),
            Var2("y"))))

    println(s"EvalR2 : ${evalR2(exp2).run(envMap)}")
    // EvalR : 45
  }

}