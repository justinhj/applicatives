package org.justinhj

object ParallelCatsEffect {
  import cats.implicits._
  import cats.data._
  import cats.{Monad,Functor}
  import cats.Applicative
  import cats.effect.{ContextShift, Timer, IO}
  import scala.concurrent.ExecutionContext
  import scala.concurrent.duration._
  import cats.effect.IO.Par

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  // Deriving map2

  case class User(email: String, name: String, blocked: Boolean)
  case class Account(email: String, balance: Long)

  def getUser(email: String) =
    IO.sleep(10 seconds) *> IO(User("bob@gmail.com", "Bob Jones", false))

  def getAccount(email: String) =
    IO.sleep(10 seconds) *> IO(Account("bob@gmail.com", 100))

  def goodStanding(user: User, account: Account): Boolean = {
    user.blocked == false &&
    account.balance >= 0
  }

  val email = "bob@gmail.com"

  val checkBob = Applicative[IO.Par].map2(
    Par(getUser(email)),
    Par(getAccount(email)))(goodStanding)

  def monadicMap2[A,B,C,F[_]: Monad](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    fa.flatMap { a =>
        fb.map(b => f(a,b))
    }

  def applicativeMap2[A,B,C,F[_]: Applicative](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
      val ffb = fa.map {
        a => (b: B) => f(a,b)
      }
      Applicative[F].ap(ffb)(fb)
    }

  // Here we explicitly use the IO's applicative
  val sampleIO = IO((a: Int) => a + 1)
                    .ap(IO(1))
                    .flatMap(n => IO(println(s"hello $n")))

  val aLotOfIOs =
    NonEmptyList.of(sampleIO, sampleIO)

  val ioOfList = aLotOfIOs.parSequence

  def main(args: Array[String]): Unit = {
    println("run parallel: " + ioOfList.unsafeRunSync)

    println("run bank check: " + Par.unwrap(checkBob).unsafeRunSync)
  }

}
