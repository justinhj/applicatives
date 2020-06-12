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

  // Tree data type
  sealed trait Exp2[+A]
  case class Val2[A](value: A) extends Exp2[A]
  case class Add2[A](left: Exp2[A], right: Exp2[A]) extends Exp2[A]
  case class Var2[A](key: String) extends Exp2[A]

  implicit val foldableExp2 = new Foldable[Exp2] {
    def foldLeft[A, B](fa: Exp2[A], b: B)(f: (B, A) => B): B = {
      // fa match {
      //   case Val2(value) => f(b, value)
      //   case Var2(key) => fetch(key)(env)
      // }
      ???
    }

    def foldRight[A, B](fa: Exp2[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
  }


  // def treeTraverse[A, B, F[_]](f: A => F[B], fs: Tree[A])
  //   (implicit app: Applicative[F]): F[Tree[B]] = {
  //   fs match {
  //     case Leaf =>
  //       app.pure(Leaf)
  //     case Node(left, a, right) =>
  //       val w1 = app.pure((l: Tree[B]) => (v: B) => (r: Tree[B]) => Node(l,v,r))
  //       val w2 = w1.ap(treeTraverse(f,left))
  //       val w3 = w2.ap(f(a))
  //       w3.ap(treeTraverse(f,right))
  //   }
  // }

  // Generic sequence (replaced IO with Applicative interface)

   def applicativeSequence2[F[_]:Applicative,A](ios: List[F[A]]): F[List[A]] = {
    val app = implicitly[Applicative[F]]
    ios match {
      case Nil =>
        app.pure(List.empty[A])
      case c :: cs =>
        app.pure((a: A) => (list: List[A]) => a +: list)
          .ap(c)
          .ap(applicativeSequence2(cs))
    }
  }


   def applicativeSequence3[F[_]:Applicative,A](ios: List[F[A]]): F[List[A]] = {
    val app = implicitly[Applicative[F]]
    ios.foldLeft(app.pure(List.empty[A])) {
      case (acc, c) =>
        app.pure((a: A) => (list: List[A]) => a +: list)
          .ap(c)
          .ap(acc)
    }
  }

  // type IntReader[A] = Reader[Int,A]
  // def temp : IntReader[Int] = for {
  //   ten <- Applicative[IntReader].pure(10).ap(Applicative[IntReader].pure((a: Int) => a + 1))
  //   r <- Reader((a: Int) => ten + 1)
  // //   .ap(Applicative[IntReader].pure((a: Int) => a + 1)).flatMap {
  // // e =>
  // //   Reader((e:Int) =>  e + 1) }.run(100)
  // } yield r

  // Not sure why ap is reversed for Reader but this has to be written backwards
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

  def main(args: Array[String]): Unit = {

    val envMap = Map("x" -> 7, "y" -> 6)

    val exp = Add(Val(10), Add(Var("x"), Var("y")))

    println(s"Eval : ${eval(exp, Env(envMap))}")
    // Eval : 23

    println(s"EvalR : ${evalR(exp).run(envMap)}")
    // EvalR : 23
  }

}