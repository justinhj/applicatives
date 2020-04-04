package org.justinhj

object Traverse {

  import cats.Applicative
  import cats.Traverse
  import cats.Monoid
  import cats.Eval
  import cats.data.Const
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

  def accumulate[A,F[_]: Traverse, B: Monoid](f: A => B)(fa: F[A]): B = {
    cats.Traverse[F].traverse(fa)((a: A) => Const.of[B](f(a))).getConst
  }

  def reduce[F[_]: Traverse, A: Monoid](fa: F[A]): A = {
    cats.Traverse[F].traverse(fa)((a: A) => Const.of[A](a)).getConst
  }

  // We need a real Cats Traverse instance for Tree for this one...
  implicit val catsTreeTraverse = new Traverse[Tree] {

    // Don't really need these to demonstrate treeFlatten, but they can be implemented
    // in terms of traverse...
    def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = ???
    def foldRight[A, B](fa: Tree[A], lb: cats.Eval[B])
      (f: (A, Eval[B]) => Eval[B]): Eval[B] = ???

    def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit app: Applicative[G]): G[Tree[B]] = {
      fa match {
            case Leaf =>
              app.pure(Leaf)
            case Node(left, a, right) =>
              val w1 = app.pure((l: Tree[B]) => (v: B) => (r: Tree[B]) => Node(l,v,r))
              val w2 = w1.ap(treeTraverse(f,left))
              val w3 = w2.ap(f(a))
              w3.ap(treeTraverse(f,right))
          }
    }
  }

  def treeFlatten[A](tree: Tree[A]): List[A] = {
    accumulate((a: A) => List(a))(tree)
  }

  def concatLists[A](fa: List[List[A]]): List[A] = {
    reduce(fa)
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

    // Traverse Tuple2
    println("listTraverse Tuple2 example: " + listTraverse((n: Int) => Tuple2(n * 2, n), List(1,2,3)))
    // listTraverse Tuple2 example: (12,List(1, 2, 3))

    // Tree traversal
    val tree1 = Node(Leaf, 10, Node(Leaf, 5, Node(Leaf, 10, Leaf)))

    println("treeTraverse: " + treeTraverse((n: Int) => Option(n + 1), tree1))
    // treeTraverse: Some(Node(Leaf,11,Node(Leaf,6,Node(Leaf,11,Leaf))))

    // Flatten a tree with Const
    println("treeTraverse const: " + treeTraverse((n: Int) => Const[Int, Boolean](n), tree1))

    // Accumulate
    println("accumulate: " + accumulate((s: String) => s.size)(List("ten", "twenty", "thirty")))
    // 15

    // Reduce
    println("reduce: " + reduce(List("ten", "twenty", "thirty")))
    // tentwentythirty

    // Tree flattening
    println("treeFlatten: " + treeFlatten(tree1))
    // treeFlatten: List(10, 5, 10)

    // Concat lists (flatten)
    println("concatLists: " + concatLists(List(List(1,2,3), List(4,5,6))))
    // concatLists: List(1, 2, 3, 4, 5, 6)


  }
}
