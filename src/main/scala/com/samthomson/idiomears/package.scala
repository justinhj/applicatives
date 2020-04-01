package com.samthomson

package object idiomears {
  def ⊏|[A, B](func: A => B) = IdiomEars.Begin(func)
  def *|[A, B](func: A => B) = IdiomEars.Begin(func)
}
