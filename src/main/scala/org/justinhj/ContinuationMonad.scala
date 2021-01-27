package org.justinhj

object ContinuationMonad {

  // What is continuation passing style

  // Take two functions...
  def inc(a: Int) = a + 1
  def double(a: Int) = a + a

  // To combine them we just compose as follows
  val a1 = double(inc(1))
  println(s"a1 = $a1")

  // For continuation passing style we want to be able just chain the functions by tell them to
  // pass their result to the rest of the computation

  def cpsInc(a: Int)(rest: Int => Unit) = {
    val res = a + 1
    rest(res)
  }

  def cpsDouble(a: Int)(rest: Int => Unit) = {
    val res = a + a
    rest(res)
  }

  // Use these in CPS style
  cpsInc(1){
    res =>
      cpsDouble(res) {
        a2 =>
          println(s"a2 $a2")
      }
  }

  // Value is a sealed trait of computed values we'll use in our Continuation example
  type Answer = Value

  /**
   * A continuation monad.
   * Implements continuation passing style as a data type
   *
   */
  case class Cont[A](in: (A => Answer) => Answer) {
    def bind[B](k: A => Cont[B]) = Cont[B](c => in (a => k(a) in c))
    def map[B](f: A => B): Cont[B] = bind(x => unitM(f(x)))
    def flatMap[B](f: A => Cont[B]): Cont[B] = bind(f)
  }

  // Given a pure value A we want to be able to continue the computation with that pure value...
  def unitM[A](a: A) = Cont[A](c => c(a))

  def showM(m: Cont[Value]): String = (m.in(identity)).toString()

  // showM(unitM("Hello World").flatMap(s => unitM(Num(s.length))))
  // res12: String = "11"

  def callCC[A](h: (A => Cont[A]) => Cont[A]) =
    Cont[A](c => h(a => Cont[A](_ => c(a))) in c)

  type Name = String

  trait Term
  case class Var(x: Name) extends Term
  case class Con(n: Int) extends Term
  case class Add(l: Term, r: Term) extends Term
  case class Lam(x: Name, body: Term) extends Term
  case class App(fun: Term, arg: Term) extends Term
  case class Ccc(x: Name, t: Term) extends Term

  trait Value
  case class Wrong(reason: String) extends Value {
   override def toString() = s"wrong: $reason"
  }
  case class Num(n: Int) extends Value {
    override def toString() = n.toString()
  }
  case class Fun(f: Value => Cont[Value]) extends Value {
    override def toString() = "<function>"
  }

  type Environment = List[(Name, Value)];

  def lookup(x: Name, e: Environment): Cont[Value] = {
    e.find{
      case (name, value) if name == x => true
      case _ => false
    } match {
      case Some((_,v)) => unitM(v)
      case _ => unitM(Wrong(s"Var $x not found"))
    }
  }

  def add(a: Value, b: Value): Cont[Value] = (a, b) match {
    case (Num(m), Num(n)) => unitM(Num(m + n))
    case _ => unitM(Wrong("Tried to add something other than two numbers"))
  }

  def apply(a: Value, b: Value): Cont[Value] = a match {
    case Fun(k) => k(b)
    case _ => unitM(Wrong("Tried to apply something other than a function"))
  }

  def interp(t: Term, e: Environment): Cont[Value] = t match {
    case Var(x) => lookup(x, e)
    case Con(n) => unitM(Num(n))
    case Add(l, r) => for (
          a <- interp(l, e);
			    b <- interp(r, e);
			    c <- add(a, b))
                      yield c
    case Lam(x, t) => unitM(Fun(a => interp(t, (x, a) :: e)))
    case App(f, t) => for (
        a <- interp(f, e);
			  b <- interp(t, e);
			  c <- apply(a, b))
		      yield c
    case Ccc(x, t) => callCC(k => interp(t, (x, Fun(k)) :: e))
  }

  def test(t: Term): String = showM(interp(t, List()))

  val term0 = App(Lam("x", Add(Var("x"), Var("x"))), Add(Con(10), Con(11)))
  val term1 = App(Con(1), Con(2))
  val term2 = Add(Con(1), Ccc("k", Add(Con(2), App(Var("k"), Con(4)))))
  val term3 = Add(Var("z"), Var("y"))

  def main(args: Array[String]) = {
    println(test(term0))
    println(test(term1))
    println(test(term2))
    println(test(term3))
  }
}