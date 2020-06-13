package org.justinhj

object MatrixTransposition {

  import cats.Applicative
  import cats.Traverse

  // Second application of applicatives is transpose

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

  def zipWith[A, B, C](as: LazyList[A], bs: LazyList[B])(
      f: (A, B) => C): LazyList[C] = {
    as.zip(bs).map { case (a, b) => f(a, b) }
  }

  def repeat[A](a: A): LazyList[A] = a #:: repeat(a)

  def transpose[A](matrix: LazyList[LazyList[A]]): LazyList[LazyList[A]] = {
    matrix match {
      case LazyList() => repeat(LazyList.empty)
      case xs #:: xss =>
        zipWith(xs, transpose(xss)) {
          case (a, as) =>
            a +: as
        }
    }
  }

  // Given repeat and zapp (see below), we can make a generalized version of the
  // binary zipWith

  // zapp :: [a → b ] → [a ] → [b ]
  // zapp (f : fs) (x : xs) = f x : zapp fs xs
  // zapp = [ ]

  def zapp[A, B](fs: LazyList[A => B])(as: LazyList[A]): LazyList[B] = {
    val zipped = fs.zip(as)
    zipped.map {
      case (f, a) => f(a)
    }
  }

  // Now we can write transpose like this

  // transpose :: [[a ]] → [[a ]]
  // transpose [ ] = repeat [ ]
  // transpose (xs : xss) = repeat (:) ‘zapp‘ xs ‘zapp‘ transpose xss

  // That last line can be written as follows to make the Scala version more
  // intuitive
  // zapp (zapp (rep (:)) xs) (zappTranspose xss)

  def transposeApp[A](matrix: LazyList[LazyList[A]]): LazyList[LazyList[A]] = {
    matrix match {
      case LazyList() => repeat(LazyList.empty)
      case xs #:: xss =>
        val fs = repeat((a: A) => (as: LazyList[A]) => a +: as)
        val zap1 = zapp(fs)(xs)
        zapp(zap1)(transposeApp(xss))
    }
  }

  implicit val implicitLazyListApplicative = new Applicative[LazyList] {
    def pure[A](a: A): LazyList[A] = repeat(a)

    def ap[A, B](ff: LazyList[A => B])(fa: LazyList[A]): LazyList[B] = {
      zapp(ff)(fa)
    }
  }

  def transposeApp2[A](matrix: LazyList[LazyList[A]]): LazyList[LazyList[A]] = {
    Traverse[LazyList].sequence(matrix)
  }

  def main(args: Array[String]): Unit = {

    val matrix = LazyList(
      LazyList(1, 2, 3, 4, 5),
      LazyList(6, 7, 8, 9, 10),
      LazyList(11, 12, 13, 14, 15)
    )

    matrix.foreach { l =>
      l.foreach { l2 =>
        print(f"$l2 ")
      }
      println()
    }

    val transposed = transpose(matrix)
    val transposedApp = transposeApp2(matrix)

    transposed.foreach { l =>
      l.foreach { l2 =>
        print(f"$l2 ")
      }
      println()
    }

    transposedApp.foreach { l =>
      l.foreach { l2 =>
        print(f"$l2 ")
      }
      println()
    }
  }

}
