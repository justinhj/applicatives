package org.justinhj

// Working through some ideas from the paper Applicative programming with effects
// by CONOR MCBRIDE, University of Nottingham and
// ROSS PATERSON, City University, London

import cats.implicits._
import cats.effect.IO

object Applicatives {

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

  // sequence :: [IO a ] → IO [a ]
  // sequence [ ] = return [ ]
  // sequence (c : cs) = return (:) ‘ap‘ c ‘ap‘ sequence cs

  def applicativeSequence[A](ios: List[IO[A]]): IO[List[A]] = {
    ios match {
      case Nil =>
        IO.pure(List.empty[A])
      // case c :: cs =>
      //   val w1 = IO.pure((a: A) => (listA: List[A]) => a +: listA)
      //   val w2 = w1.ap(c)
      //   val w3 = w2.ap(sequence(cs))
      //   w3
      case c :: cs =>
        IO.pure((a:A) => (as:List[A])
            => a +: as)
          .ap(c)
          .ap(sequence(cs))
    }
  }

  // which every Monad must provide, lifts pure values to the effectful world,
  // whilst ap provides ‘application’ within it

  // Except for the noise of the returns and aps, this definition is in a fairly standard
  // applicative style, even though effects are present.

  // Justin Note: what does applicative style mean here?

  def printIO(out: String): IO[Unit] = {
    IO.delay(println(out))
  }

  def main(args: Array[String]): Unit = {

    val ios = List(
      printIO("Why"),
      printIO("Hello"),
      printIO("Mother"))

    println("list created")
    val prog = sequence(ios).replicateA(5)

    println("prog created")
    prog.unsafeRunSync()

    println("Let's apply ourselves ;)")

    val progA = applicativeSequence(ios)
    progA.unsafeRunSync()

    println("fin")
  }

}
