package org.justinhj

// Working through some ideas from the paper Applicative programming with effects
// by CONOR MCBRIDE, University of Nottingham and
// ROSS PATERSON, City University, London
// https://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

object SequencingCommands {

  import cats._
  import cats.data.Const
  import cats.data.Nested
  import cats.effect.ContextShift
  import cats.effect.IO
  import cats.effect.IO.Par
  import cats.implicits._
  import scala.concurrent.ExecutionContext
  import scala.concurrent.duration._

  // Some things needed by Cats Effect
  implicit val timer = IO.timer(ExecutionContext.global)
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  // The paper begins noting that "One often wants to execute a sequence of commands and
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

  // We could avoid the need for names to wire these values through to their
  // point of usage if we had a kind of ‘effectful application’.

  // ap is mentioned which you can find in the Haskell Control library
  // https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
  // ap :: Monad m => m (a -> b) -> m a -> m b

  // pure, which every Monad must provide, lifts pure values to the effectful world,
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

  def applicativeSequence2[A](ios: List[IO[A]]): IO[List[A]] = {
    ios match {
      case Nil =>
        IO.pure(List.empty[A])
      case c :: cs =>
        IO.pure((a: A) => (list: List[A]) => a +: list)
          .ap(c)
          .ap(applicativeSequence2(cs))
    }
  }

  def genericSequence[F[_], A](ios: List[F[A]])
    (implicit app: Applicative[F])
    : F[List[A]] = {
    ios match {
      case Nil =>
        app.pure(List.empty[A])
      case c :: cs =>
        app.pure((a: A) => (list: List[A]) => a +: list)
          .ap(c)
          .ap(genericSequence(cs))
    }
  }

  // Note that the default instance for IO is a monad, and ap is implemented with flatMap
  // to get actual parallelism we need the applicative instance of IO.
  // To select the applicative instance we need to use IO.Par.

  def parApplicativeSequence[A](ios: List[IO.Par[A]])
    (implicit applicative : Applicative[IO.Par]):
      IO.Par[List[A]] = {
    ios match {
      case Nil =>
        Par(IO.pure(List()))
      case c :: cs =>
        Par(IO.pure((a: A) => (listA: List[A]) => a +: listA))
          .ap(c)
          .ap(parApplicativeSequence(cs))
    }
  }

  def printIO(out: String): IO[Unit] = {
    for (
      _ <- IO.sleep(1 second);
      _ <- IO(println(out))
    ) yield ()
  }

  // Replace IO with Applicative
  // Replace recursion with foldLeft
  def applicativeSequence3[F[_]:Applicative,A](ios: List[F[A]]): F[List[A]] = {
    val app = implicitly[Applicative[F]]
    ios.foldLeft(app.pure(List.empty[A])) {
      case (acc, c) =>
        app.pure((a: A) => (list: List[A]) => a +: list)
          .ap(c)
          .ap(acc)
    }
  }

  // Implement to traverse
  //   def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def applicativeTraverse1[G[_]:Applicative,A,B](fa: List[A])(f: A => G[B]): G[List[B]] = {
    val app = implicitly[Applicative[G]]
    fa.foldLeft(app.pure(List.empty[B])) {
      case (acc, c) =>
        app.pure((b: B) => (list: List[B]) => b +: list)
          .ap(f(c))
          .ap(acc)
    }.map(_.reverse)
  }

  // Note that we can't replace list with a generic type, the :+ and reverse part is
  // not available to generic types, but we can fold if we have a monoid in G
  // def applicativeTraverse2[F[_],G[_],A,B](fa: F[A])(f: A => G[B])
  //   (implicit foldable: Foldable[F], applicative: Applicative[G], monoid: Monoid[G[_]]): G[F[B]] = {

  //   fa.foldLeft(applicative.pure(monoid.empty[B])) {
  //     case (acc, c) =>
  //       app.pure((b: B) => (bs: G[B]) => ???)
  //         .ap(f(c))
  //         .ap(acc)
  //   }
  // }

  // Replace list with Foldable
  // def applicativeSequence4[F[_]:Applicative, G[_]: Foldable,A](ios: G[F[A]]): F[G[A]] = {
  //   val app = implicitly[Applicative[F]]
  //   ios.foldLeft(app.pure(G.empty[A])) {
  //     case (acc, c) =>
  //       app.pure((a: A) => (gs: G[A]) => a +: gs)
  //         .ap(c)
  //         .ap(acc)
  //   }
  // }


  type ConstIOStringResult[A] = Const[(Long, Int, String), A]
  type IOResult[A] = Tuple3[Long, Int, A]

  def exampleIO(n: Int) : IO[IOResult[String]] = {
    IO.sleep(n seconds).map(s => (n.toLong, 1, n.toString))
  }

  val now: IO[Long] = IO(System.currentTimeMillis)

  def time[A](ioa: IO[A]): IO[(Long, A)] = for {
      start <- now
      a <- ioa
      end <- now
    } yield ((end - start), a)


  def exampleIO2(n: Int): IO[String] = {
    IO(println(s"Starting $n")) *>
    IO.sleep(n seconds) *>
    IO(println(s"Completed $n")) *>
    IO.pure(n.toString)
  }

    val testios = List(1,2,3)


  // IO[Const[(Long, Int, String),String]]

  val test= Nested[IO,Const[(Long, Int, String),?], String](time(exampleIO2(3)).map{case (time, a) => Const((time, 1, a))})

  val result = Traverse[List].traverse(testios) {
    n =>
      Nested[IO, ConstIOStringResult, String](
        exampleIO(n).map(r => Const(r))
      )
  }

  val result2 = Traverse[List].traverse(testios) {
    n =>
      Nested[IO.Par, ConstIOStringResult, String](
        Par(exampleIO(n)).map(r => Const(r))
      )
  }

  val result3 = Traverse[List].traverse(List(1,2,3,4,8)) {
    n =>
      Nested[IO.Par,Const[(Long, Int, List[String], List[Long]),?], String](
        Par(time(exampleIO2(n)).map{case (time, a) => Const((time, 1, List(a), List(time)))})
      )
  }

//  Traverse[List].traverse(List(1,2,3,4))((a:Int) => Const[Int,Any](a))
// res2: Const[Int, List[Any]] = Const(10)

  def main(args: Array[String]): Unit = {

    val resultRun = Par.unwrap(result3.value).unsafeRunSync().getConst
    println(s"result: $resultRun")

    println(s"Total execution time (ms): ${resultRun._1}")
    println(s"Average time per operation (ms): ${resultRun._1 / resultRun._2}")

    System.exit(0)

    val ios = List(
      printIO("Why,"),
      printIO("hello"),
            printIO("hello1"),

                  printIO("hello2"),

                        printIO("hello3"),

                              printIO("hello4"),

                                    printIO("hello5"),

                                          printIO("hello"),

      printIO("there!"))

    Par(IO((a: Int) => a + 1)).ap(Par(IO{println("hello"); 1}))


    Applicative[IO.Par].ap(Par(IO((a: Int) => a + 1)))(Par(IO{println("hello"); 1}))

    def traverseDemo1 = {
      Par.unwrap(
       Traverse[List].traverse(ios)(io => Par(io))
      )
    }

    println("traverse.sequence...")
    val prog = Traverse[List].traverse(ios)(Par(_))
    Par.unwrap(prog).unsafeRunSync()

    // Note this is a monad so you can now use all the functions such as .replicateA(5)

    //println("prog created")
    //prog.unsafeRunSync()

    // println("Let's apply ourselves 3 ;)")

    // val progA = applicativeSequence3(ios)
    // progA.unsafeRunSync()

    println("Let's traverse ;)")

    val progT = applicativeTraverse1(List(1,2,3))(a => Option(a))
    println(s"traversed option $progT")

    // // Using Cats sequence
    // val progCList = NonEmptyList.fromListUnsafe(ios)
    // val progC = progCList.parSequence
    // progC.unsafeRunSync()

    // println("Here we go again!")

    // // Using parallel sequence
    // val parIos = ios.map(Par.apply)
    // val progB = parApplicativeSequence(parIos)
    // Par.unwrap(progB).unsafeRunSync()

    // println("fin")

  }
}