object MyFunctions {
  def factorial(num: Int): Int = {
    @annotation.tailrec
    def go(n: Int, accum: Int): Int =
      if (n <= 1) accum
      else go(n - 1, n * accum)

    go(num, 1);
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n == 0) a
      else if (n == 1) b
      else go(n - 1, b, a + b)

    go(n, 0, 1)
  }

  // Finds the first element in an array of any type that satisfies the given fn
  def findFirst[A] (arr: Array[A], fn: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= arr.length) -1
      else if (fn(arr(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A] (arr: Array[A], cmp: (A, A) => Boolean): Boolean = {
    def loop(n: Int) : Boolean =
      if (n >= arr.length - 1) true
      else if (!cmp(arr(n), arr(n+1))) false
      else loop(n + 1)

    loop(0)
  }

  def partial1[A, B, C] (a: A, fn: (A, B) => C): B => C =
    (b) => fn(a, b)

  def curry[A,B,C](fn: (A, B) => C): A => (B => C) =
    (a) => (b) => fn(a, b)

  def uncurry[A,B,C](fn: A => B => C): (A, B) => C =
    (a, b) => fn(a)(b)

  def compose[A, B, C](fn: B => C, gn: A => B): A => C =
    (a) => fn(gn(a))
}
