package com.samthomson.idiomears

import cats.Applicative

//import scala.language.higherKinds


/**
  * Idiom brackets a la "Applicative programming with effects", McBride and
  * Paterson.
  * Allows the following syntax for `Applicative`s like `Option`, `List`, etc.:
  *
  *   > val f = (a: Int) => (b: Int) => (s: String) => s + (a * b)
  *   > ⊏| (f) (2.some) (5.some) ("a".some) |⊐
  *   Some("a10")
  *   > val g = (a: Int) => (b: Int) => a * b
  *   > ⊏| (g) (List(1, 2)) (List(3, 4)) |⊐
  *   List(3, 4, 6, 8)
  *   > ⊏| (g) (1.some) (none) |⊐
  *   None
  *
  * The implicit classes and other contortions are to make sure no methods
  * have implicit arguments, so we can string together chains of function
  * applications without Scala thinking the next arg is an implicit.
  */
object IdiomEars {
  /** open bracket, start of an idiomatic expression */
  def ⊏|[A, B](func: A => B) = Begin(func)
  /** ascii alias for `⊏|` */
  def *|[A, B](func: A => B) = ⊏|(func)

  case class Begin[A, B](func: A => B) {
    // we don't want to infer which idiom `F` we're in until we see the 1st arg.
    def apply[F[_]](a: F[A]) = inside[F].apply(a)
    def inside[F[_]] = Inside[F, A => B](_.pure(func))
  }

  /**
   * We still delay asking for the `Applicative[F]` until we the end,
   * where we can do it inside an implicit conversion.
   * if `A` is a function, we can continue the expression using `ContinueOp.apply`.
   * otherwise we can end it with `EndOps.|⊐` or `|*`.
   */
  case class Inside[F[_], A](build: Applicative[F] => F[A])

  implicit class ContinueOp[F[_], A, B, C](a: Inside[F, A])(implicit ev: F[A] <:< F[B => C]) {
    def apply(b: F[B]) = Inside[F, C](F => F.ap(a.build(F))(b))
  }

  implicit class EndOps[F[_], A](a: Inside[F, A])(implicit F: Applicative[F]) {
    /** close bracket, end of idiomatic expression */
    val |⊐ = a.build(F)
    /** ascii alias for `|⊐` */
    val |* = |⊐
  }
}
