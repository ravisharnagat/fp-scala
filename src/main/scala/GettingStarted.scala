import scala.annotation.tailrec

object GettingStarted extends App {

//  def main(args: Array[String]): Unit =
    println(fib(5))

  def binarySearch(list: Array[Int], k: Int): Int = { //  binarySearch(Array(1,4,6,9, 12), 12)
    @tailrec
    def loop(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid = (low + high) / 2
        val x = list(mid)
        if (x == k) mid
        else if (x > k) loop(0, mid - 1)
        else loop(mid + 1, high)
      }
    }
    loop(0, list.length - 1)
  }

  def isSorted(as: Array[Int], gt: (Int, Int) => Boolean): Boolean = { //  isSorted(Array(1,4,6, 9, 12).reverse, (a,b) => a > b)
    @tailrec
    def loop(start: Int, end: Int, check: Boolean): Boolean = {
      if (start == as.length - 1) true
      else if (gt(as(start), as(end))) {
        loop(start + 1, end + 1, check)
      }
      else false
    }
    loop(0, 1, false)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(iterator: Int, prev: Int, cur: Int): Int = {
      if(iterator == 0) prev
      else if(iterator == 1) cur
      else loop(n-1, cur, cur+prev)
    }

    loop(n, 0, 1)
  }

  def partial[A,B,C](a: A, f: (A, B) => C): B => C = (b: B) => f(a,b)

  def curry[A,B,C](f: (A,B) => C): A => B => C = (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A,b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  val f = (x: Double) => math.Pi / 2 - x
  val cos = f andThen math.sin

  compose(f, cos)(4)

 }
