package org.justinhj

object Traverse {

  import cats.Applicative
  import cats.implicits._

  def dist[A, F[_]](fs: List[F[A]])(implicit app: Applicative[F]): F[List[A]] = {
    fs match {
      case Nil =>
        app.pure(List.empty[A])
      case c :: cs =>
        val w1 = app.pure((a: A) => (listA: List[A]) => a +: listA)
        val w2 = w1.ap(c)
        w2.ap(dist(cs))
    }
  }

  def main(args: Array[String]): Unit = {

    // dist a list of options
    println(dist(List(Option(10), Option(10), Option(3), Option(4))))
    // Some(List(10, 10, 3, 4))

    // Note that we have short circuiting
    println(dist(List(None, Option(10), Option(3), Option(4))))
    // None



  }
}
