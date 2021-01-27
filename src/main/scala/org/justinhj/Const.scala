package org.justinhj

object Const {

  import cats.Applicative
  import cats.Monoid
  import cats.Traverse
  import cats.implicits._

  // Exploring Const and the applicative functor

  // newtype Const b a = Const{unConst :: b}
  // instance Monoid b ⇒ Applicative (Const b) where
  // pure = Const /0
  // x⊛y = Const (unConst x⊕unConst y)

  // When Everything Fits: The Beauty of Composition - Markus Hauck
  // From the video https://youtu.be/sHV4qhbZHgo

  // We can make an Applicative effect into a Monoid as follows

  implicit def appMonoid[A: Monoid, F[_]: Applicative] = new Monoid[F[A]] {
    def empty: F[A] = Applicative[F].pure((Monoid[A].empty))
    def combine(x: F[A], y: F[A]): F[A] =
      Applicative[F].map2(x,y)(Monoid[A].combine)
  }

  // By implementing the monoid of the applicative we can append using the
  // idiomatic effect of the effect type

  // appMonoid[Int, Option].combine(appMonoid[Int, Option].empty, Some(10))
  // res: Option[Int] = Some(10)

  // appMonoid[Int, List].combine(List(1,2,3), List(4,5,6))
  // res: List[Int] = List(5, 6, 7, 6, 7, 8, 7, 8, 9)

  // To go the other way, Monoid to Applicative we need Const...

  // Let's make our own const data type. A is the phantom type
  // Can be seen as a typelevel version of
  // `Function.const[A, B]: A => B => A`

  final case class MyConst[A,B](unConst: A)

  def unConst[A,B](c: MyConst[A,B]) = c.unConst

  // Monoid instance

  implicit def myConstMonoid[A: Monoid, B] = new Monoid[MyConst[A,B]] {
    val m = implicitly[Monoid[A]]
    def empty = MyConst(m.empty)
    def combine(x: MyConst[A,B], y: MyConst[A,B]) =
      MyConst(x.unConst.combine(y.unConst))
  }

  // We can't implement the Applicative for Const without cheating and
  // having a Monoid for X. Otherwise we could not implement pure. Pure
  // requires summoning an X when we don't have one, but since X
  // is a Monoid we can use empty...
  implicit def appConst[X: Monoid] = new Applicative[MyConst[X,?]] {

    // Note that map ignores the phantom and just returns a Const
    // with the "real" or non-Phantom value. Effectively
    // we ignore the function A => B but go from Const[X,A] to Const[X,B]
    // without losing the value of X...
    override def map[A, B](fa: MyConst[X,A])(f: A => B): MyConst[X,B] = MyConst(fa.unConst)

    // This cannot be implemented
    // def flatMap[A,B](fa: MyConst[X,A])(f: A => MyConst[X,B]): MyConst[X,B] = {
    //   val x = fa.unConst
    //   val a = ???
    //   // f(a)
    //   ???
    // }

    override def pure[A](x: A): MyConst[X,A] = MyConst(Monoid[X].empty)

    override def ap[A, B](ff: MyConst[X,A => B])(fa: MyConst[X,A]): MyConst[X,B] =
      MyConst(ff.unConst |+| fa.unConst)
  }

  val c1 = MyConst[String, Double]("Oh hai")
  val c2 = MyConst[String, Double]("Goodbye")

  // "Const lifts any Monoid into an Applicative"
  // "Option lifts any Semigroup" ?

  // Tuple2 applicative
  implicit def appTuple2[X: Monoid] = new Applicative[Tuple2[X,?]] {
    def pure[A](a: A): (X, A) = (Monoid[X].empty, a)

    def ap[A, B](ff: (X, A => B))(fa: (X, A)): (X, B) = {
      (ff._1.combine(fa._1),
        ff._2(fa._2))
    }
  }

  def main(args: Array[String]): Unit = {
    println(Applicative[MyConst[String, ?]].map2(c1,c2)((a,b) => a + b))
    // MyConst(Oh haiGoodbye)

    // Traverse a list of ints to sum them
    println(Traverse[List].traverse(List(1,2,3,4,5))(a => MyConst[Int, String](a)).unConst)

    // Count the total characters in all strings
    println(Traverse[List].traverse(List(
      "A",
      "List",
      "Of",
      "Various",
      "Strings"
      ))(a => MyConst[Int, Boolean](a.size)).unConst)

    // Applicative ap for tuple
    // The function to apply is a tuple where _1 is a value of type
    // X and _2 is A => B
    // The end result is an (X,B)
    // where X combines the the X values of ff and fa
    // and the B values comes from applying the function to the
    // _2 of the fa
    // def ap[A, B](ff: (X, A => B))(fa: (X, A)): (X, B) = {
    //   val x = s.combine(ff._1, fa._1)
    //   val b = ff._2(fa._2)
    //   (x, b)
    // }
    // And pure
    // Given an A we use the empty value of the monoid for _1
    // and a is _2 for a Tuple2[X,A]
    // def pure[A](a: A): (X, A) = (MX.empty, a)

    // Use Tuple2 instead of MyConst
    println(
      Traverse[List].traverse(List(1,2,3,4,5,6,7))(a => Tuple2(a, true))._1
    )
    // 15

    // Counting find items...
    // Traverse[List].traverse(List(1,2,3,4,3))((n: Int) => if(n == 3) Const.of[String](1) else Const.of[String](0))
  }
}
