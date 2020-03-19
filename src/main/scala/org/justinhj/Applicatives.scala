package org.justinhj

import cats.CommutativeApplicative

// Working through some ideas from the paper Applicative programming with effects
// by CONOR MCBRIDE, University of Nottingham and
// ROSS PATERSON, City University, London
// https://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

object Applicatives {

  // import cats._
  import cats.effect.ContextShift
  import cats.Applicative
  // import cats.syntax.parallel._
  import cats.effect.IO
  import cats.effect.IO.Par
  import cats.implicits._
  import scala.concurrent.ExecutionContext
  import scala.concurrent.duration._
//  import cats.data.NonEmptyLazyList

  // Some things needed by Cats Effect
  implicit val timer = IO.timer(ExecutionContext.global)
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  // The paper begins noting that often we want to "One often wants to execute a sequence of commands and
  // collect the sequence of their responses"

  // sequence :: [IO a ] → IO [a ]
  // sequence [ ] = return [ ]
  // sequence (c : cs) = do
  //   x ← c
  //   xs ← sequence cs
  //   return (x : xs)

  def sequence[A](ios: List[IO[A]]): IO[List[A]] = {
    ios match {
      case Nil =>
        IO.pure(List.empty[A])
      case c :: cs =>
        for (
          x <- c;
          xs <- sequence(cs)
        ) yield (x +: xs)
    }
  }

  // ap is mentioned which you can find in the Haskell Control library
  // https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
  // ap :: Monad m => m (a -> b) -> m a -> m b

  // We could avoid the need for names to wire these values through to their
  // point of usage if we had a kind of ‘effectful application’.

  // which every Monad must provide, lifts pure values to the effectful world,
  // whilst ap provides ‘application’ within it

  // Except for the noise of the returns and aps, this definition is in a fairly standard
  // applicative style, even though effects are present.

  // Justin Note: what does applicative style mean here?

  // sequence :: [IO a ] → IO [a ]
  // sequence [ ] = return [ ]
  // sequence (c : cs) = return (:) ‘ap‘ c ‘ap‘ sequence cs

  def applicativeSequence[A](ios: List[IO[A]]): IO[List[A]] = {
    ios match {
      case Nil =>
        IO.pure(List.empty[A])
      case c :: cs =>
        val w1 = IO((a: A) => (listA: List[A]) => a +: listA)
        val w2 = w1.ap(c)
        val w3 = w2.ap(applicativeSequence(cs))
        w3
    }
  }

  // Note that the default instance for IO is a monad, and ap is implemented with flatMap
  // to get actual parallelism we need the applicative instance of IO.
  // To select the applicative instance we need to use IO.Par.

  def parApplicativeSequence[A](ios: List[IO.Par[A]])
    (implicit applicative : CommutativeApplicative[IO.Par]):
      IO.Par[List[A]] = {
    ios match {
      case Nil =>
        Par(IO.pure(List()))
      case c :: cs =>
        Par(IO((a: A) => (listA: List[A]) => a +: listA))
          .ap(c)
          .ap(parApplicativeSequence(cs))
    }
  }

  // Now we get into some applications of applicative. First one is
  // matrix tranpose

  // EG:
  // List((1,2,3),(4,5,6),(7,8,9))

  // You can do this in applicative style, and it uses Haskell's zipWith
  // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

  // In Scala zipWith[A,B,C](f: (A,B) => C, fa: List[A], fb: List[B]): List[C]

  // transpose :: [[a ]] → [[a ]]
  // transpose [ ] = repeat [ ]
  // transpose (xs : xss) = zipWith (:) xs (transpose xss)

  // There is no zipWith in Scala, and we need a lazy list so we need to implement
  // zipWith for LazyList

  def zipWith[A,B,C](as: LazyList[A], bs: LazyList[B])(f: (A,B) => C): LazyList[C] = {
    as.zip(bs).map{case (a,b) => f(a,b)}
  }

  def repeat[A](a: A): LazyList[A] = a #:: repeat(a)

  def transpose[A](matrix: LazyList[LazyList[A]]): LazyList[LazyList[A]] = {
    matrix match {
      case LazyList() => repeat(LazyList.empty)
      case xs #:: xss =>
        zipWith(xs,transpose(xss)) {
          case (a, as) =>
            a +: as
        }
    }
  }

  // Given repeat and zapp (see below), we can make a generalized version of the
  // binary zipWith
  def zapp[A,B](fs: LazyList[A => B])(as: LazyList[A]): LazyList[B] = {
    val zipped = fs.zip(as)
    zipped.map {
      case (f, a) => f(a)
    }
  }

  def printIO(out: String): IO[Unit] = {
    for (
      _ <- IO.sleep(500 milliseconds);
      _ <- IO(println(out))
    ) yield ()
  }

  def main(args: Array[String]): Unit = {

    val ios = List(
      printIO("Why"),
      printIO("Hello"),
      printIO("Mother Seconds"))

    println("list created")
    // val prog = sequence(ios)
    // Note this is a monad so you can now use all the functions such as .replicateA(5)

    //println("prog created")
    //prog.unsafeRunSync()

    println("Let's apply ourselves ;)")

    // val progA = applicativeSequence(ios)
    // progA.unsafeRunSync()

    // // Using Cats sequence
    // val progCList = NonEmptyList.fromListUnsafe(ios)
    // val progC = progCList.parSequence
    // progC.unsafeRunSync()

    // Using parallel sequence
    val parIos = ios.map(Par.apply)
    val progB = Par.unwrap(parApplicativeSequence(parIos))
    progB.unsafeRunSync()

    println("fin")

    // transpose

    val matrix = LazyList(LazyList(1,2,3,4,5),LazyList(6,7,8,9,10),LazyList(11,12,13,14,15))
    matrix.foreach {
      l =>
        l.foreach{
          l2 =>
            print(s"$l2 ")
        }
        println()
      }

    val transposed = transpose(matrix)
    transposed.foreach {
      l =>
        l.foreach{
          l2 =>
            print(s"$l2 ")
        }
        println()
      }
    }

}
