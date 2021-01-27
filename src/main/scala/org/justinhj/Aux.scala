package org.justinhj

object Aux extends App {
  trait Last[A] {
    type B
    def last(a: A): B
  }

  // The type B always depends on the type A, that's why A is an input type of the typeclass and B is an output type.


  object Last {
    type Aux[A,B0] = Last[A] { type B = B0 }

    implicit def tuple1Last[A]: Aux[Tuple1[A],A] = new Last[Tuple1[A]] {
      type B = A
      def last(a: Tuple1[A]) = a._1
    }

    implicit def tuple2Last[A,C]: Aux[Tuple2[A,C],C] = new Last[(A,C)] {
      type B = C
      def last(a: (A,C)) = a._2
    }

    implicit def listLast[A]: Aux[List[A],A] = new Last[List[A]] {
      type B = A
      def last(a: List[A]) = a.reverse.head
    }

    implicit def listToLast[A](list: List[A]): Last[List[A]] = {
      listLast
    }

    implicit def tuple2ToLast[A,B](tuple2: Tuple2[A,B]): Last[Tuple2[A,B]] = {
      tuple2Last
    }
  }

//  val t2 = Tuple2("Hello", 12)
//  println(s"last of t2 is ${t2.last}")

  val l1 = List("Hello", "Mr", "Aux")
  println(s"last of l1 is ${l1.last}")

  // Scala 3 has dependent types
//  trait Last[A] {
//    type B
//    def last(a: A): B
//  }
//
//  object Last {
//    given [A] as Last[Tuple1[A]] {
//      type B = A
//      def last(a: Tuple1[A]) = a._1
//    }
//
//    given [A, C] as Last[(A,C)] {
//      type B = C
//      def last(a: (A,C)) = a._2
//    }
//  }


}
