sealed trait MyList[+A]

case object Nil extends MyList[Nothing]

case class Cons[+A](x: A, xs: MyList[A]) extends MyList[A]

//https://scastie.scala-lang.org/EdeY58WTSj2oA9jbBBml8A

object MyList {
  def sum(xs: MyList[Int]): Int = {
    xs match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }
  }

  def product(xs: MyList[Int]): Int = {
    xs match {
      case Nil => 1
      case Cons(h, t) => h * product(t)
    }
  }

  def apply[A](xs: A*): MyList[A] = {
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, f(h, z))(f))
    }
  }
  
  def foldLeft[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def reverseListUsingFoldLeft[A](l: MyList[A]): MyList[A] = {
    foldLeft(l, MyList[A]())((acc, h) => Cons(h, acc))
  }



  //Ex15
  def concat[A](l: MyList[MyList[A]]): MyList[A] =
    foldRight(l, Nil: MyList[A])(append)

  //Ex16
  def add1(l: MyList[Int]): MyList[Int] =
    foldRight(l, Nil: MyList[Int])((h, t) => Cons(h + 1, t))

  //Ex17
  def doubleToString(l: MyList[Double]): MyList[String] =
    foldRight(l, Nil: MyList[String])((h, t) => Cons(h.toString, t))

  def map_v1[A, B](l: MyList[A])(f: A => B): MyList[B] = {
    foldRight(l, MyList[B]())((h, t) => Cons(f(h), t))
  }

  def map[A, B](l: MyList[A])(f: A => B): MyList[B] = {
    l match {
      case Nil => ()
      case Cons(h, t) =>
    }
  }


  //MAP AND FLATMAP
  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => {
        val k = append(t, a2)
        println(k)
        Cons(h, k)
      }
    }
  }

  def map[A, B](l: MyList[A])(f: A => B): MyList[B] = {
    foldRight(l, MyList[B]())((h, t) => Cons(f(h), t))
  }

  def concat[A](l: MyList[MyList[A]]): MyList[A] = {
    foldRight(l, MyList[A]())(append)
  }

  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] = {
    concat(map(l)(f))
  }


  //HasSubsequence
  def startsWith[A](l: MyList[A], prefix: MyList[A]): Boolean = {
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, _)) => if (h1 == h2) true else startsWith(t1, prefix)
      case _ => false
    }
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

}
