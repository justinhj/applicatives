object Scala2EvalNumeric extends App {

  // An expression evaluator using Scala 2

  import Scala2Numeric._
  import Scala2Numeric.Numeric.ops._

  sealed trait Exp[T]
  case class Val[T](value: T) extends Exp[T]
  case class Add[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class Mul[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class Var[T](identifier: String) extends Exp[T]

  type Env[T] = Map[String, T]

  def eval[T : Numeric](exp: Exp[T])(implicit env : Env[T]): T = {
    exp match {
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l,r) => handleAdd(l,r)
      case Mul(l,r) => handleMul(l,r)
    }
  }

  def handleAdd[T](l: Exp[T], r: Exp[T])(implicit env : Env[T], numeric: Numeric[T]) = eval(l) + eval(r)
  def handleMul[T](l: Exp[T], r: Exp[T])(implicit env : Env[T], numeric: Numeric[T]) = eval(l) * eval(r)
  def handleVar[T](s: String)(implicit env: Env[T]): T = env.get(s).get

  def sumList[T](ts: List[T])(implicit numeric : Numeric[T]): T = {
    ts.reduce((a,b) => numeric.Add(a,b))
  }


  {
    val exp1 : Exp[Int] = Mul(Var("z"), Add(Val(30), Mul(Var("x"), Var("y"))))

    implicit val intEnv : Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)
    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  {
    implicit val longEnv : Env[Long] = Map("x" -> 17, "y" -> 10, "z" -> 2)
    val eval2 = eval(Mul(Val(3L), Val(10L)))

    println(s"Eval2 exp gives $eval2")
    println(s"sumlist ${sumList(List(1,2,3,4))}")

  }

  {
    implicit val stringEnv : Env[String] = Map("x" -> "a", "y" -> "b")
    val eval3 = eval(Mul(Val("ab"), Val("cd")))

    println(s"Eval3 exp gives $eval3")

    println(s"sumlist ${sumList(List("1111111", "2222", "333"))}")
  }
}
