object Scala2Numeric {

  object Numeric {
    // This can be used to summon a numeric (same as implicitly)
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

    object ops {

      implicit class NumericOps[T](a: T)(implicit n: Numeric[T]) {
        def Add(b: T): T = n.Add(a, b)

        def +(b: T): T = n.Add(a, b)

        def Mul(b: T): T = n.Mul(a, b)

        def *(b: T): T = n.Mul(a, b)
      }

    }

  }

  trait Numeric[T] {
    def Add(a: T, b: T): T

    def Mul(a: T, b: T): T
  }

  implicit val numericInt: Numeric[Int] = new Numeric[Int] {

    def Add(a: Int, b: Int): Int = a + b

    def Mul(a: Int, b: Int): Int = a * b
  }

  implicit val numericLong: Numeric[Long] = new Numeric[Long] {

    def Add(a: Long, b: Long): Long = a + b

    def Mul(a: Long, b: Long): Long = a * b
  }

  implicit val numericString: Numeric[String] = new Numeric[String] {

    def Add(a: String, b: String): String = a ++ b

    def Mul(a: String, b: String): String = {
      for (as <- a;
           bs <- b;
           s <- as.toString ++ bs.toString) yield s
    }

  }
}