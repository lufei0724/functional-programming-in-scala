package c01.start

//** MyModule
// Simple scala program */
object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFibo(n: Int) = {
    val msg = "The fibonacci value of %d is %d"
    msg.format(n, fibo(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def fibo(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, prev: Int, curr: Int): Int = {
      if (i == n) curr
      else go(i + 1, curr, prev + curr)
    }

    if (n == 0) 0
    else go(1, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n > as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(prev: Int, curr: Int): Boolean = {
      if (curr == as.length) true
      else if (ordered(as(prev), as(curr))) loop(curr, curr + 1)
      else false
    }

    loop(0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("ABS", -2, abs))
    println(formatResult("Fibonacci", 6, fibo))

    val list = Array(1, 3, 2, 4, 5)
    println(isSorted(list, (a: Int, b: Int) => a < b))
  }
}
