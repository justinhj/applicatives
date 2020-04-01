package org.justinhj

object IdiomEars {

  import cats.implicits._
  import com.samthomson.idiomears._

  // Demo of https://github.com/sammthomson/IdiomEars

  def main(args: Array[String]): Unit = {
    val f = (a: Int) => (b: Int) => a * b
    println( ⊏| (f) (List(1, 2)) (List(3, 4)) |⊐ )
  }
}