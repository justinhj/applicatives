package org.justinhj

object Traverse {

  import cats.Applicative
  import cats.data.{Validated, NonEmptyList}
  import cats.implicits._

  // applicative distributor for lists
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

  def flakyMap[A,B](f: A => Option[B], as: List[A]): Option[List[B]] = {
    dist(as.map(f))
  }

  // Traversal of a list
  def listTraverse[A, B, F[_]](f: A => F[B], fs: List[A])
    (implicit app: Applicative[F]): F[List[B]] = {
    fs match {
      case Nil =>
        app.pure(List.empty[B])
      case c :: cs =>
        val w1 = app.pure((b: B) => (listB: List[B]) => b +: listB)
        val w2 = w1.ap(f(c))
        w2.ap(listTraverse(f, cs))
    }
  }

  // dist, or really it's now known as sequence is traverse with the identity function
  def sequence[A, F[_]](fs: List[F[A]])
    (implicit app: Applicative[F]): F[List[A]] = {
    listTraverse((fa: F[A]) => fa, fs)
  }

  // Traversal of a tree

  sealed trait Tree[+A]
  case object Leaf extends Tree[Nothing]
  case class Node[A](left: Tree[A], a: A, right: Tree[A]) extends Tree[A]

  def treeTraverse[A, B, F[_]](f: A => F[B], fs: Tree[A])
    (implicit app: Applicative[F]): F[Tree[B]] = {
    fs match {
      case Leaf =>
        app.pure(Leaf)
      case Node(left, a, right) =>
        val w1 = app.pure((l: Tree[B]) => (v: B) => (r: Tree[B]) => Node(l,v,r))
        val w2 = w1.ap(treeTraverse(f,left))
        val w3 = w2.ap(f(a))
        w3.ap(treeTraverse(f,right))
    }
  }

  def main(args: Array[String]): Unit = {

    // dist a list of options
    println("dist option success: " + dist(List(Option(10), Option(10), Option(3), Option(4))))
    // dist option success: Some(List(10, 10, 3, 4))

    // Note that we have short circuiting
    println("dist option failure: " + dist(List(None, Option(10), Option(3), Option(4))))
    // dist option failure: None

    // Validated can accumulate errors instead of failing fast...
    val someValidateds: List[Validated[NonEmptyList[String],Int]] =
      (List("Some error".invalidNel, 10.valid, "Another error".invalidNel, 4.valid))

    // Try the same with Validated that has an Applicative instance
    println("Validated failure: " + dist(someValidateds))
    // Validated failure example: Invalid(NonEmptyList(Had some error, Another error))

    // Flakymap ...
    println("Flakymap success: " + flakyMap((n: Int) => Option(n * 2), List(1,2,3)))
    // Flakymap success: Some(List(2, 4, 6))

    println("Flakymap failure: " + flakyMap((n: Int) => if(n%2==1) Some(n) else None, List(1,2,3)))
    // Flakymap failure: None

    // Traverse combines dist and map to remove the double traversal of flakyMap
    println("listTraverse example: " + listTraverse((n: Int) => Option(n * 2), List(1,2,3)))
    // Output is the same as flakyMap

    // Sequence
    println("sequence: " + sequence(List(Option(10), Option(10), Option(3), Option(4))))
    // sequence: Some(List(10, 10, 3, 4))

    // Tree traversal
    val tree1 = Node(Leaf, 10, Node(Leaf, 5, Node(Leaf, 10, Leaf)))

    println("treeTraverse: " + treeTraverse((n: Int) => Option(n + 1), tree1))
    // treeTraverse: Some(Node(Leaf,11,Node(Leaf,6,Node(Leaf,11,Leaf))))
  }
}
